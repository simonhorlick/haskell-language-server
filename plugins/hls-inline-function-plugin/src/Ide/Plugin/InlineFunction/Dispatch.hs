-- | Build the dispatch-preserving rewrites used when no single clause of
-- a definition is decidable at a call site (tier 2): instead of splicing
-- one clause's body, splice an expression that keeps the clause dispatch
-- itself. For a definition
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
module Ide.Plugin.InlineFunction.Dispatch
  ( dispatchRewrites
  ) where

import           Data.Generics      (listify)
import qualified Data.Set           as S
import           Retrie.ExactPrint  (TransformT, pruneA, setEntryDP)
import qualified Retrie.Expr
import           Retrie.Expr        (mkApps, mkLams, mkLocA, mkLocatedHsVar,
                                     mkVarPat)
import           Retrie.GHC
import           Retrie.Quantifiers (mkQs)
import           Retrie.Types       (Rewrite, mkRewrite)

-- | The two dispatch rewrites for the given function binding. The site
-- policy in "Ide.Plugin.InlineFunction.Resolve" only routes a span to
-- these when the clause that fires there cannot be decided; the applied
-- form can only match a fully-applied prefix call and the bare form only
-- a lone reference, so both can share one span set.
dispatchRewrites
  :: LocatedN RdrName
  -> MatchGroup GhcPs (LHsExpr GhcPs)
  -> TransformT IO [Rewrite (LHsExpr GhcPs)]
dispatchRewrites funId mg =
  case unLoc (mg_alts mg) of
    [] -> pure []
    alts@(L _ firstAlt : _) -> do
      let arity = length (matchPats firstAlt)
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
  caseTok   <- EpTok <$> Retrie.Expr.mkAnchor (SameLine 0)
  ofTok     <- EpTok <$> Retrie.Expr.mkAnchor (SameLine 1)
  altsL     <- mkLocA (SameLine 0) altsList
  let mg = mkMatchGroup (Generated OtherExpansion SkipPmc) altsL
  mkLocA (SameLine 1) (HsCase (EpAnnHsCase caseTok ofTok) (setEntryDP scrut (SameLine 1)) mg)

-- | The scrutinee: the lone argument variable, or a tuple of all of
-- them. Exact-printing supplies a tuple expression's commas itself, so
-- unlike 'mkTuplePat' the components carry none.
mkScrutinee :: [LHsExpr GhcPs] -> TransformT IO (LHsExpr GhcPs)
mkScrutinee [q] = pure q
mkScrutinee qs = do
  open  <- Retrie.Expr.mkAnchor (SameLine 0)
  close <- Retrie.Expr.mkAnchor (SameLine 0)
  args  <- withCommas (zipWith reEnter [0 :: Int ..] qs)
  mkLocA (SameLine 1) $
    ExplicitTuple (open, close) (map (Present noExtField) args) Boxed
  where
    reEnter i q = setEntryDP q (SameLine (min i 1))

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
  pat <- case matchPats m of
    [p] -> pure (setEntryDP p (SameLine 0))
    ps  -> mkTuplePat ps
  mkLocA (DifferentLine 1 (if i == 0 then 2 else 0)) $
    Match noExtField CaseAlt (L (EpaSpan noSrcSpan) [pat]) (rarrows (m_grhss m))

-- | @(p0, p1)@ from the clause's parameter patterns.
mkTuplePat :: [LPat GhcPs] -> TransformT IO (LPat GhcPs)
mkTuplePat ps = do
  open  <- Retrie.Expr.mkAnchor (SameLine 0)
  close <- Retrie.Expr.mkAnchor (SameLine 0)
  ps'   <- withCommas (zipWith reEnter [0 :: Int ..] ps)
  mkLocA (SameLine 0) (TuplePat (open, close) ps' Boxed)
  where
    -- first component flush after '(', later ones a space after the comma
    reEnter i p = setEntryDP p (SameLine (min i 1))

-- | Add a trailing comma annotation to every element but the last.
withCommas :: [LocatedA e] -> TransformT IO [LocatedA e]
withCommas []       = pure []
withCommas [x]      = pure [x]
withCommas (x : xs) = (:) <$> addComma x <*> withCommas xs
  where
    addComma (L (EpAnn anc (AnnListItem ts) cs) e) = do
      comma <- AddCommaAnn . EpTok <$> Retrie.Expr.mkAnchor (SameLine 0)
      pure (L (EpAnn anc (AnnListItem (ts <> [comma])) cs) e)

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
