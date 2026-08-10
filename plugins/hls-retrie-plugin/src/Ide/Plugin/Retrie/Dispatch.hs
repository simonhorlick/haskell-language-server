{-# LANGUAGE CPP #-}
-- | Build the dispatch-preserving rewrites used when no single clause of
-- a definition is decidable at a call site: instead of splicing one clause's
-- body, splice an expression that keeps the clause dispatch itself. For a
-- definition
--
-- > f 0 y = a
-- > f x y = b
--
-- two rewrites are produced:
--
-- * a fully-applied form, @f q0 q1@ ==> @case (q0, q1) of (0, y) -> a;
--   (x, y) -> b@ (no tuple at arity one), and
-- * a bare-reference form, @f@ ==> @\\x0 x1 -> case (x0, x1) of ...@,
--   covering partial-application references like @map f xs@.
--
-- The case alternatives are the definition's own clauses, patterns,
-- guards and where blocks verbatim -- only the match context changes
-- (the @=@ separators become @->@). Scrutinising a tuple of the
-- arguments has exactly a 'MatchGroup'\'s semantics: each alternative
-- matches the components left to right and falls through to the next on
-- a mismatch, and the tuple construction itself is lazy, so nothing is
-- forced that the function would not force.
module Ide.Plugin.Retrie.Dispatch
  ( dispatchRewrites
  , triviallySelectable
  ) where

import qualified Data.Foldable                             as F
import           Data.Generics                             (listify)
import           Retrie.ExactPrint                         (TransformT)
import           Retrie.GHC
import           Retrie.Types                              (Rewrite)

#if MIN_VERSION_ghc(9,12,0)
import qualified Data.Set                                  as S
import           Development.IDE.GHC.ExactPrint.Annotation (ensureTrailingComma)
import           Retrie.ExactPrint                         (pruneA, setEntryDP)
import           Retrie.Expr                               (mkAnchor, mkApps,
                                                            mkLams, mkLocA,
                                                            mkLocatedHsVar,
                                                            mkVarPat)
import           Retrie.Quantifiers                        (mkQs)
import           Retrie.Types                              (mkRewrite)
#endif

-- | A definition whose sole clause fires at every call site and whose
-- body substitutes cleanly: exactly one clause, all-variable parameter
-- patterns, a single unguarded right-hand side, and no record-wildcard
-- construction. Splicing that clause's body needs no dispatch.
--
-- A @R {..}@ construction in the body references the parameters it
-- captures invisibly to retrie's substitution, so such a body cannot be
-- spliced directly even when the clause always fires; the dispatch form
-- keeps the binding patterns alongside the body.
triviallySelectable :: MatchGroup GhcPs (LHsExpr GhcPs) -> Bool
triviallySelectable mg = case unLoc (mg_alts mg) of
  [L _ m] ->
    all isVarPat (matchClausePats m)
      && unguarded (m_grhss m)
      && not (usesWildcardCon m)
  _ -> False
  where
    isVarPat p = case unLoc p of
      VarPat{} -> True
      _        -> False
    -- grhssGRHSs is a list before GHC 9.14 and a NonEmpty after
    unguarded :: GRHSs GhcPs (LHsExpr GhcPs) -> Bool
    unguarded g = case F.toList (grhssGRHSs g) of
      [L _ (GRHS _ [] _)] -> True
      _                   -> False
    wildcardCon :: HsRecFields GhcPs (LHsExpr GhcPs) -> Bool
    wildcardCon HsRecFields{rec_dotdot = Just{}} = True
    wildcardCon _                                = False
    usesWildcardCon = not . null . listify wildcardCon

-- | @m_pats@ as a plain list: GHC 9.12 wrapped the pattern list of a
-- 'Match' in an outer 'Located'.
matchClausePats :: Match GhcPs (LHsExpr GhcPs) -> [LPat GhcPs]
#if MIN_VERSION_ghc(9,12,0)
matchClausePats = unLoc . m_pats
#else
matchClausePats = m_pats
#endif

#if MIN_VERSION_ghc(9,12,0)

-- | The two dispatch rewrites for the given function binding. The
-- applied form can only match a fully-applied prefix call and the bare
-- form only a lone reference, so handing both to retrie for the same
-- call site is safe: at most one of them fires there.
dispatchRewrites
  :: LocatedN RdrName
  -> MatchGroup GhcPs (LHsExpr GhcPs)
  -> TransformT IO [Rewrite (LHsExpr GhcPs)]
dispatchRewrites funId mg =
  case unLoc (mg_alts mg) of
    [] -> pure []
    alts@(L _ firstAlt : _) -> do
      let arity = length (matchClausePats firstAlt)
          names = freshArgNames arity mg
      applied <- appliedRewrite funId names alts
      bare    <- bareRewrite funId names alts
      pure [applied, bare]

-- | @f q0 q1@ ==> @case (q0, q1) of ...@, quantified over the argument
-- variables (retrie substitutes the call site's argument expressions for
-- them on both sides).
appliedRewrite
  :: LocatedN RdrName
  -> [RdrName]
  -> [LMatch GhcPs (LHsExpr GhcPs)]
  -> TransformT IO (Rewrite (LHsExpr GhcPs))
appliedRewrite funId names alts = do
  fe    <- mkLocatedHsVar funId
  qs    <- mapM mkVarExpr names
  lhs   <- mkApps fe qs
  caseE <- mkCaseOf names alts
  p     <- pruneA lhs
  t     <- pruneA caseE
  pure (mkRewrite (mkQs names) p t)

-- | @f@ ==> @\\x0 x1 -> case (x0, x1) of ...@. The binder names are
-- fresh with respect to every name the clauses mention, so the lambda
-- cannot capture anything the alternatives reference.
bareRewrite
  :: LocatedN RdrName
  -> [RdrName]
  -> [LMatch GhcPs (LHsExpr GhcPs)]
  -> TransformT IO (Rewrite (LHsExpr GhcPs))
bareRewrite funId names alts = do
  fe    <- mkLocatedHsVar funId
  caseE <- mkCaseOf names alts
  pats  <- mapM (\n -> mkVarPat =<< mkLocA (SameLine 0) n) names
  lam   <- mkLams pats caseE
  p     <- pruneA fe
  t     <- pruneA lam
  pure (mkRewrite (mkQs []) p t)

-- | @case (x0, x1) of@ over the given argument variables, with one
-- alternative per clause.
mkCaseOf
  :: [RdrName]
  -> [LMatch GhcPs (LHsExpr GhcPs)]
  -> TransformT IO (LHsExpr GhcPs)
mkCaseOf names alts = do
  scrut     <- mkScrutinee =<< mapM mkVarExpr names
  altsList  <- sequence (zipWith (\i -> clauseToAlt i . unLoc) [0 ..] alts)
  caseTok   <- EpTok <$> mkAnchor (SameLine 0)
  ofTok     <- EpTok <$> mkAnchor (SameLine 1)
  altsL     <- mkLocA (SameLine 0) altsList
  let mg = mkMatchGroup (Generated OtherExpansion SkipPmc) altsL
  mkLocA (SameLine 1) (HsCase (EpAnnHsCase caseTok ofTok) (setEntryDP scrut (SameLine 1)) mg)

-- | The scrutinee: the lone argument variable, or a tuple of all of
-- them.
mkScrutinee :: [LHsExpr GhcPs] -> TransformT IO (LHsExpr GhcPs)
mkScrutinee [q] = pure q
mkScrutinee qs = do
  open  <- mkAnchor (SameLine 0)
  close <- mkAnchor (SameLine 0)
  args  <- tupleComponents qs
  mkLocA (SameLine 1) $
    ExplicitTuple (open, close) (map (Present noExtField) args) Boxed

-- | Convert one clause of the definition to a case alternative: the
-- parameter patterns become the (tuple) pattern, the right-hand sides
-- keep their guards and where block and swap their @=@ for @->@.
--
-- The first alternative's newline-and-indent establishes the layout
-- column of the alternative list; its siblings' deltas are relative to
-- that column, so they carry no further indent.
clauseToAlt
  :: Int
  -> Match GhcPs (LHsExpr GhcPs)
  -> TransformT IO (LMatch GhcPs (LHsExpr GhcPs))
clauseToAlt i m = do
  pat <- case matchClausePats m of
    [p] -> pure (setEntryDP p (SameLine 0))
    ps  -> mkTuplePat ps
  mkLocA (DifferentLine 1 (if i == 0 then 2 else 0)) $
    Match noExtField CaseAlt (L (EpaSpan noSrcSpan) [pat]) (rarrows (m_grhss m))

-- | @(p0, p1)@ from the clause's parameter patterns.
mkTuplePat :: [LPat GhcPs] -> TransformT IO (LPat GhcPs)
mkTuplePat ps = do
  open  <- mkAnchor (SameLine 0)
  close <- mkAnchor (SameLine 0)
  ps'   <- tupleComponents ps
  mkLocA (SameLine 0) (TuplePat (open, close) ps' Boxed)

-- | The comma'd, spaced components of a synthesized tuple, shared by
-- the scrutinee expression and the alternative patterns: the first
-- component sits flush after @(@, later ones a space after their
-- comma.
tupleComponents :: [LocatedA e] -> TransformT IO [LocatedA e]
tupleComponents xs = pure (withCommas (zipWith reEnter [0 :: Int ..] xs))
  where
    reEnter i x = setEntryDP x (SameLine (min i 1))

-- | Add a trailing comma annotation to every element but the last.
withCommas :: [LocatedA e] -> [LocatedA e]
withCommas []       = []
withCommas [x]      = [x]
withCommas (x : xs) = addComma x : withCommas xs
  where
    addComma (L l e) = L (ensureTrailingComma l) e

-- | Swap each right-hand side's @=@ separator for @->@, keeping its
-- position (and any guard bars) so the clause's spacing survives.
rarrows :: GRHSs GhcPs (LHsExpr GhcPs) -> GRHSs GhcPs (LHsExpr GhcPs)
rarrows g = g { grhssGRHSs = fmap fixGrhs (grhssGRHSs g) }
  where
    fixGrhs (L l (GRHS ann guards body)) = L l (GRHS (fixAnn ann) guards body)
    fixGrhs g                            = g
    fixAnn (EpAnn anc (GrhsAnn vbar sep) cs) =
      EpAnn anc (GrhsAnn vbar (Right (asRarrow sep))) cs
    asRarrow (Left (EpTok loc)) = EpUniTok loc NormalSyntax
    asRarrow (Left NoEpTok)     = EpUniTok (EpaDelta noSrcSpan (SameLine 1) []) NormalSyntax
    asRarrow (Right r)          = r

mkVarExpr :: RdrName -> TransformT IO (LHsExpr GhcPs)
mkVarExpr name = mkLocatedHsVar =<< mkLocA (SameLine 1) name

-- | Argument names that collide with nothing the clauses mention, so
-- they can bind (bare form) or quantify (applied form) freely.
freshArgNames :: Int -> MatchGroup GhcPs (LHsExpr GhcPs) -> [RdrName]
freshArgNames n mg =
  take n [mkVarUnqual (fsLit nm) | nm <- candidates, nm `S.notMember` used]
  where
    used = S.fromList
      [ occNameString (rdrNameOcc r)
      | r <- listify (\(_ :: RdrName) -> True) mg
      ]
    candidates = ["x", "y", "z"] <> ["x" <> show i | i <- [0 :: Int ..]]

#else

-- | The pre-9.12 exact-print annotation API differs enough that the
-- dispatch construction is not supported there; returning no rewrites
-- makes the caller fall back to the per-clause rewrites (the
-- pre-dispatch behavior).
dispatchRewrites
  :: LocatedN RdrName
  -> MatchGroup GhcPs (LHsExpr GhcPs)
  -> TransformT IO [Rewrite (LHsExpr GhcPs)]
dispatchRewrites _ _ = pure []

#endif
