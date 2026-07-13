{-# LANGUAGE CPP #-}
module Ide.Plugin.InlineFunction.Resolve
  ( nameUnderCursor
  , findDefinition
  , findAllCallSites
  , selectClauseSites
  , clauseRefsFor
  , callSiteAt
  , InlineCandidate(..)
  , BindingDef(..)
  , ClauseDef(..)
  , CallSite(..)
  , SiteInline(..)
  , CursorSite(..)
  ) where

import           Control.Monad                  (guard)
import           Data.Generics                  (Data, everything, extQ,
                                                 listify, mkQ)
import           Data.List                      (nub, sortOn)
import qualified Data.Map                       as M
import           Data.Maybe                     (isJust, listToMaybe, mapMaybe)
import qualified Data.Set                       as S
import           Development.IDE.Core.RuleTypes (HieAstResult (..))
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Error      (realSrcSpanToRange)
import           Development.IDE.Spans.AtPoint  (pointCommand)
import           GHC.Iface.Ext.Types            (ContextInfo (..), HieAST,
                                                 IdentifierDetails (identInfo))
import qualified GHC.Iface.Ext.Types            as Hie
import           Ide.Plugin.InlineFunction.Util (grhsList, hsVarName, location,
                                                 matchPats, refSpellings,
                                                 toRealSrcSpan)
import           Language.LSP.Protocol.Types    (Position, Range (..))

-- | A fully-applied call to the function: span of the entire applied
-- expression, plus argument spans (in source order).
data CallSite = CallSite
  { application :: !RealSrcSpan
    -- ^ Span of the whole call the rewrite replaces.
  , headRef     :: !RealSrcSpan
    -- ^ Span of the reference to the function heading the call -- the
    -- name at the front of the application or between the backticks.
    -- Equals 'application' for a bare-reference (partial application)
    -- site.
  , arguments   :: ![RealSrcSpan]
    -- ^ Spans of the arguments.
  , argExprs    :: ![LHsExpr GhcRn]
    -- ^ The argument expressions themselves, in the same order as
    -- 'arguments'; clause selection inspects their shape.
  , prefixApp   :: !Bool
    -- ^ Whether the site is a prefix application (or bare reference) of
    -- the function, as opposed to a backtick call or section. Only
    -- prefix shapes can host the dispatch rewrite.
  , inlineVia   :: !SiteInline
    -- ^ How this site is rewritten. Assigned by 'selectClauseSites';
    -- 'SiteClause' 0 until then.
  }

-- | How a call site is inlined.
data SiteInline
  = SiteClause !Int
    -- ^ Splice the body of this clause (index into the definition's
    -- 'clauses'): the clause provably fires at this site.
  | SiteDispatch
    -- ^ No single clause is decidable here; splice a case expression
    -- that keeps the definition's whole dispatch.
  deriving stock (Eq, Ord)

-- | A resolved candidate for inlining.
data InlineCandidate = InlineCandidate
  { name       :: !Name
    -- ^ Name of the function. Used to populate the code actions menu.
  , definition :: !BindingDef
    -- ^ The definition of the function we want to inline.
  , sites      :: ![CallSite]
    -- ^ Sites to rewrite.
  }

-- | The function being inlined.
data BindingDef = BindingDef
  { clauses      :: ![ClauseDef]
    -- ^ The definition's clauses, in source order (non-empty). At least
    -- one clause is selectable, or the definition is no candidate.
  , arity        :: !Int
    -- ^ Number of parameters; every clause of a 'MatchGroup' has the
    -- same count. Determines the call-site arity to look for.
  , dispatchable :: !Bool
    -- ^ Whether a site where no single clause is decidable may be
    -- rewritten to a case expression carrying the whole dispatch
    -- (tier 2): every clause passes 'dispatchOk' and there is at least
    -- one parameter to scrutinise.
  , funIdSpan    :: !RealSrcSpan
    -- ^ Span of the @fun_id@ (the function name at its binding site). This
    -- can differ from 'nameSrcSpan' of the bound 'Name' -- for example, a
    -- type class default method's 'Name' is declared by the type signature
    -- in the class header, but the @fun_id@ of the default-method FunBind
    -- is at the implementation site.
  , neededExts   :: ![Extension]
    -- ^ Language extensions the definition's syntax needs wherever it is
    -- spliced: a @..@ record wildcard, a @\\case@ lambda, a multi-way
    -- @if@, a view pattern. The splice carries the syntax verbatim
    -- (bodies always, patterns inside a dispatch), and it only parses
    -- where the extension is on -- which the plugin cannot enable at a
    -- target module.
  }

-- | One clause (one 'Match') of the function being inlined.
data ClauseDef = ClauseDef
  { clausePats :: ![LPat GhcRn]
    -- ^ The clause's parameter patterns, in source order.
  , selectable :: !Bool
    -- ^ Whether this clause may be inlined at a call site: its patterns
    -- are within the rewrite-supported subset ('rewritablePat'), its
    -- body is a single unguarded right-hand side, and it passes the
    -- per-clause body checks (no self-reference, no forall'd type
    -- variable, no record-wildcard construction from a parameter). An
    -- unselectable clause still takes part in clause selection: a call
    -- site whose arguments definitely do not match it may skip past it
    -- to a later clause, but a site it definitely or possibly matches
    -- cannot be inlined.
  , dispatchOk :: !Bool
    -- ^ Whether this clause may travel inside a spliced dispatch (a
    -- case alternative): no self-reference and no forall'd type
    -- variable. Patterns, guards and where blocks are copied verbatim
    -- there, so none of the other 'selectable' restrictions apply.
  , clauseRefs :: ![(RdrName, Name)]
    -- ^ External names the clause body (including its where clause)
    -- references, each paired with the spelling the source text uses --
    -- the spliced code keeps that spelling, so a qualified reference
    -- needs its qualifier in scope at the splice point. Parameters and
    -- body-local binders are internal names, so they are excluded
    -- automatically.
  , patRefs    :: ![(RdrName, Name)]
    -- ^ External names the clause's parameter patterns reference --
    -- data constructors, mostly. A clause splice consumes the patterns
    -- by matching, but a dispatch copies them verbatim into its case
    -- alternative, so there they need to resolve at the target like
    -- body references do.
  }

-- | Where the cursor stood when the candidate was found: on a use of the
-- function, or on its binding. Only a use identifies a single call site,
-- so only there can an \"inline this use site\" action be offered.
data CursorSite = AtUseSite | AtDefinition
  deriving stock Eq

-- | The term-level identifier under the cursor, provided it stands at a spot
-- where inlining makes sense: a use of the name or its binding site.
--
-- Note: This function should be fast as it is called frequently.
nameUnderCursor :: HieAstResult -> Position -> Maybe (Name, CursorSite)
nameUnderCursor HAR{hieAst} pos = do
  -- extract the identifiers under the cursor
  let names = concat $ pointCommand hieAst pos extractNames
  -- restrict to identifiers at valid inline sites
  (name, ctxs, _) <- listToMaybe $ filter
    (\(n, ctxs', _) ->
      isInlineSite ctxs' &&
      -- omit identifiers that are types
      not (isTyConName n || isTyVarName n))
    names
  let site = if any isUse ctxs then AtUseSite else AtDefinition
  pure (name, site)
  where
    isUse = \case
      Use -> True
      _   -> False

-- | Search the defining module's renamed source for @name@'s binding and
-- check it has a form we can inline.
findDefinition :: RenamedSource -> Name -> Maybe BindingDef
findDefinition rn name = checkBinder =<< findBinder rn name

-- | What one generic pass over the renamed source yields for
-- 'findAllCallSites': the candidate call nodes in traversal order, plus
-- the spans its site-policy filters consume.
data Scan = Scan
  { scanCandidates :: [LHsExpr GhcRn]
    -- ^ Nodes that may head a call site of the function.
  , scanOpSpans    :: [RealSrcSpan]
    -- ^ References in infix-operator position ('OpApp' and operator
    -- sections); never independent sites.
  , scanTyAppHeads :: [RealSrcSpan]
    -- ^ Heads of visible type applications; never rewritten.
  , scanProcSpans  :: [RealSrcSpan]
    -- ^ Arrow-notation @proc@ blocks; nothing inside one is rewritten.
  }

instance Semigroup Scan where
  Scan a b c d <> Scan a' b' c' d' =
    Scan (a <> a') (b <> b') (c <> c') (d <> d')

instance Monoid Scan where
  mempty = Scan [] [] [] []

-- | Find all call sites of @name@ in the renamed source.
--
-- A call site is either fully applied (@length args == arity@) or a bare
-- @HsVar@ reference with no arguments (@null args@). The latter covers both
-- arity-0 definitions (where the bare reference /is/ the call) and partial
-- references like @map double [1, 2, 3]@ for a 1-ary @double@. Retrie's
-- rewrite construction emits an arity-0 rewrite alongside the fully-applied
-- one, so a bare reference inlines as an eta-expanded lambda.
findAllCallSites :: RenamedSource -> Name -> Int -> [CallSite]
findAllCallSites rn name arity =
    filter (not . insideProc) $
      dropTypeApplied $
        dropShadowed $
          dropOperatorRefs $
            mapMaybe toCallSite candidates
  where
    -- everything the classification below consumes is collected in one
    -- generic pass over the renamed source
    Scan candidates opSpanList tyAppHeadList procSpanList =
      everything (<>) ((mempty `mkQ` scanExpr) `extQ` scanLocated) rn
    opSpans        = S.fromList opSpanList
    tyAppHeadSpans = S.fromList tyAppHeadList
    procSpans      = procSpanList

    scanLocated :: LHsExpr GhcRn -> Scan
    scanLocated node =
      mempty
        { scanCandidates = [node | isCandidate node]
        , scanTyAppHeads = [sp | HsAppType _ fun _ <- [unLoc node], Just sp <- [location fun]]
        , scanProcSpans  = [sp | HsProc{} <- [unLoc node], Just sp <- [location node]]
        }

    -- matched on the unlocated payload: the renamer stores a section's
    -- original expression bare inside an 'XExpr' expansion, where a
    -- located query never fires
    scanExpr :: HsExpr GhcRn -> Scan
    scanExpr expr =
      mempty
        { scanOpSpans = [sp | Just op <- [operatorOf expr], Just sp <- [location op]]
        }

    -- an 'OpApp' node allows for infix calls like @1 `add` 2@. every 'HsVar'
    -- of @name@ is a candidate -- 'toCallSite' decides whether it stands
    -- alone (a partial reference) or sits inside a larger application we
    -- already match via 'HsApp'. 'HsPar' is a candidate because a
    -- parenthesized call is a site of its own (see below).
    isCandidate node =
      case unLoc node of
        HsApp{}       -> arity > 0
        OpApp{}       -> arity == 2
        HsPar{}       -> True
        HsVar _ ident -> hsVarName ident == name
        _             -> False

    -- the reference in operator position of an infix application or an
    -- operator section; feeds 'dropOperatorRefs'
    operatorOf :: HsExpr GhcRn -> Maybe (LHsExpr GhcRn)
    operatorOf = \case
      OpApp _ _ op _  -> Just op
      SectionL _ _ op -> Just op
      SectionR _ op _ -> Just op
      _               -> Nothing
    toCallSite candidate =
      case unLoc candidate of
        -- A parenthesized call: the site's span must be the paren node's,
        -- because retrie matches through parens and emits its replacement
        -- there. The inner expression also matches on its own;
        -- 'dropShadowed' removes that duplicate. Sections are the
        -- exception: they stay parenthesized in HsSyn (retrie never
        -- matches through their parens), so a section site keeps the
        -- section node's own span, where the replacement lands.
        HsPar _ inner -> do
          site <- toCallSite inner
          if isJust (sectionParts inner)
            then pure site
            else do
              appSp <- location candidate
              pure site { application = appSp }
        -- backtick infix application: lhs `name` rhs
        OpApp _ lhs op@(L _ (HsVar _ ident)) rhs
          | hsVarName ident == name ->
              mkCallSite False candidate op [lhs, rhs]
        -- an operator section -- @(`name` e)@ or @(e `name`)@ -- supplies
        -- one argument; retrie's section rewrites eta-expand the
        -- remaining parameters, so the section inlines to a lambda
        -- spliced inside the source parens.
        _ | Just (op, operand) <- sectionParts candidate
          , L _ (HsVar _ ident) <- op
          , hsVarName ident == name
          , arity >= 2 ->
              mkCallSite False candidate op [operand]
        _ -> case collectArgs candidate of
          -- a fully-applied call, or a bare 'HsVar' reference (zero args)
          -- that retrie will eta-expand.
          (hd@(L _ (HsVar _ ident)), args)
            | hsVarName ident == name
            , length args == arity || null args ->
                mkCallSite True candidate hd args
          -- an infix call that also supplies extra arguments:
          -- @(1 `e` 2) 3@ passes the first two arguments through the
          -- operator and the rest by ordinary application
          (headExpr, extras)
            | L _ (OpApp _ lhs op@(L _ (HsVar _ ident)) rhs) <- unparen headExpr
            , hsVarName ident == name
            , not (null extras)
            , 2 + length extras == arity ->
                mkCallSite False candidate op (lhs : rhs : extras)
          _ -> Nothing

    -- A bare reference standing in the operator position of an infix
    -- application or an operator section is not an independent call
    -- site: eta-expanding a lambda into operator position is not even
    -- syntactically valid. The enclosing 'OpApp' (or the whole
    -- application around it) is the site, when the arity fits.
    dropOperatorRefs sites =
      filter
        (\s -> not (null s.arguments && s.application `S.member` opSpans))
        sites

    -- A bare reference under a visible type application -- @e \@Int 5@ --
    -- is never rewritten: the eta-expanded lambda retrie substitutes for
    -- a bare reference cannot be type-applied, and dropping the
    -- application would change how the type is instantiated. This runs
    -- after 'dropShadowed' so a parenthesized head -- @(e) \@Int@ -- is
    -- already collapsed to the paren-spanned site the 'HsAppType' holds.
    dropTypeApplied sites =
      filter
        (\s -> not (null s.arguments && s.application `S.member` tyAppHeadSpans))
        sites

    -- Arrow command syntax restricts where a spliced expression may
    -- stand, so uses inside a 'proc' block are never inlined.
    insideProc site =
      any (`containsSpan` site.application) procSpans
    -- Remove the inner duplicate of a parenthesized call: same argument
    -- spans, application span properly contained in the other's.
    dropShadowed sites = filter (\s -> not (any (`shadows` s) sites)) sites
      where
        shadows t s =
          arguments t == arguments s &&
          application t /= application s &&
          application t `containsSpan` application s

-- | Build a 'CallSite' from the whole applied expression, the reference
-- heading it, and the argument expressions. Clause selection has not run
-- yet, so the site points at the first clause.
mkCallSite :: Bool -> LHsExpr GhcRn -> LHsExpr GhcRn -> [LHsExpr GhcRn] -> Maybe CallSite
mkCallSite prefix whole hd args = do
  appSp  <- location whole
  headSp <- location hd
  argSps <- traverse location args
  pure
    CallSite
      { application = appSp
      , headRef     = headSp
      , arguments   = argSps
      , argExprs    = args
      , prefixApp   = prefix
      , inlineVia   = SiteClause 0
      }

unparen :: LHsExpr GhcRn -> LHsExpr GhcRn
unparen (L _ (HsPar _ inner)) = unparen inner
unparen le                    = le

-- | The call site under the cursor. When call sites nest -- @e (e 2)@ --
-- the innermost one containing the position wins.
--
-- 'findAllCallSites' also records each bare reference to the function as a
-- site (the partial-application case). A bare reference that is the head
-- of an enclosing call site is not an independent site -- retrie rewrites
-- the enclosing application, so selecting the bare head would filter
-- every replacement away. Drop exactly those before choosing; a bare
-- reference standing anywhere else inside another call (in one of its
-- arguments, say) remains selectable on its own.
callSiteAt :: Position -> [CallSite] -> [CallSite]
callSiteAt pos sites =
  take 1 $ sortOn size $ filter contains independent
  where
    independent = filter (not . subsumedBareRef) sites
    subsumedBareRef site =
      null site.arguments &&
      any
        (\other ->
          other.application /= site.application &&
          other.headRef == site.application)
        sites
    contains site =
      let Range start end = rangeOf site.application
      in start <= pos && pos <= end
    rangeOf = realSrcSpanToRange
    size site =
      let sp = site.application
      in ( srcSpanEndLine sp - srcSpanStartLine sp
         , srcSpanEndCol sp - srcSpanStartCol sp
         )

-- | View an expression as an operator section, looking through the
-- expansion wrapper the renamer stores the original section in.
-- Returns the operator and the supplied operand.
sectionParts :: LHsExpr GhcRn -> Maybe (LHsExpr GhcRn, LHsExpr GhcRn)
sectionParts (L _ expr) =
  case expr of
    XExpr (ExpandedThingRn (OrigExpr orig) _) -> go orig
    _                                         -> go expr
  where
    go = \case
      SectionL _ operand op -> Just (op, operand)
      SectionR _ op operand -> Just (op, operand)
      _                     -> Nothing

-- In the AST a fully-applied function 'f 1 2' takes the form
-- 'App (App (Var f) (Lit 1)) (Lit 2)', we want the function 'f' and the
-- list of args '1', '2', etc.
collectArgs :: LHsExpr GhcRn -> (LHsExpr GhcRn, [LHsExpr GhcRn])
collectArgs node = go [] node
  where
    go args n =
      case n of
        (L _ (HsApp _ fn arg)) -> go (arg : args) fn
        other                  -> (other, args)

-- True if the binding has the given name and it is a function-like binding.
isBindingNamed :: Name -> LHsBindLR GhcRn GhcRn -> Bool
isBindingNamed name bind =
  case unLoc bind of
    FunBind{fun_id = L _ n} -> n == name
    _                       -> False

isVarNamed :: Name -> LHsExpr GhcRn -> Bool
isVarNamed name var =
  case unLoc var of
    HsVar _ ident -> hsVarName ident == name
    _             -> False

findBinder :: RenamedSource -> Name -> Maybe (LHsBindLR GhcRn GhcRn)
findBinder rn name =
  listToMaybe $ listify (isBindingNamed name) rn

singletonToMaybe :: [a] -> Maybe a
singletonToMaybe = \case
  [x] -> Just x
  _   -> Nothing

-- Check that the function body is suitable for inlining.
-- A non-empty @where@ clause is allowed: retrie's 'matchToRewrites'
-- wraps the body in a @let@ that re-binds the where-clause names, so
-- the inlined call site preserves them.
checkBody :: GRHSs GhcRn (LHsExpr GhcRn) -> Maybe (LHsExpr GhcRn)
checkBody GRHSs{grhssGRHSs} = do
  -- assert exactly one right-hand side
  rightHandSide <- singletonToMaybe $ grhsList grhssGRHSs
  case unLoc rightHandSide of
    -- check the rhs has no guards
    GRHS _ [] body -> Just body
    _              -> Nothing

-- | Patterns the rewrite layer can turn into a retrie query (retrie's
-- @patToExpr@), restricted further to those whose match is purely
-- structural -- so a 'PatYes' verdict from 'matchPat' coincides with the
-- clause's retrie template actually matching the call site. Notably
-- excluded: as-, bang-, lazy- and view patterns, @n+k@, record-syntax
-- constructor patterns, and pattern synonyms (whose match semantics are
-- hidden behind the synonym).
rewritablePat :: LPat GhcRn -> Bool
rewritablePat lpat =
  case unLoc lpat of
    VarPat{}            -> True
    WildPat{}           -> True
    ParPat _ p          -> rewritablePat p
    TuplePat _ ps Boxed -> all rewritablePat ps
    ListPat _ ps        -> all rewritablePat ps
    LitPat{}            -> True
    NPat{}              -> True
    ConPat _ lcon details
      | isDataConName (hsVarName lcon) ->
          case conSubPats details of
            Just ps -> all rewritablePat ps
            Nothing -> False
    _                   -> False

-- | The sub-patterns of a constructor pattern, in match order; 'Nothing'
-- for record syntax, whose field order is not the match order.
conSubPats :: HsConPatDetails GhcRn -> Maybe [LPat GhcRn]
conSubPats = \case
#if __GLASGOW_HASKELL__ >= 913
  PrefixCon ps   -> Just ps
#else
  PrefixCon _ ps -> Just ps
#endif
  InfixCon p1 p2 -> Just [p1, p2]
  RecCon{}       -> Nothing

-- Finds uses of 'p'
findVars :: Data a => Name -> a -> [RealSrcSpan]
findVars p node =
  mapMaybe location $
    listify (isVarNamed p) node

-- | True when the spliced code references a type variable -- a 'HsTyVar'
-- whose 'Name' is itself a type variable (rather than a type
-- constructor). The only way a type variable can be referenced from
-- expression-level code is via a forall'd type variable brought into
-- scope by 'ScopedTypeVariables' (or the implicit forall in the
-- function's own type signature). Inlining at a call site where the
-- corresponding forall'd variable is unbound or bound to something
-- else would change its meaning, so we refuse to offer the action.
referencesTypeVar :: Data a => a -> Bool
referencesTypeVar node =
  any isTyVarRef $ listify (\(_ :: HsType GhcRn) -> True) node
  where
    isTyVarRef :: HsType GhcRn -> Bool
    isTyVarRef (HsTyVar _ _ ident) = isTyVarName (hsVarName ident)
    isTyVarRef _                   = False

-- | True when the body constructs a record with a wildcard ('R {..}')
-- whose implicit fields pick up one of the given binders. The renamer
-- expands the wildcard into the fields it binds, marking where the
-- implicit ones start, and each implicit field's value is a bare
-- variable reference -- so the check is: any implicit field whose
-- value is one of the binders.
wildcardUsesBinder :: Data a => [Name] -> a -> Bool
wildcardUsesBinder binders node =
  any usesBinder (listify isRecordCon node)
  where
    binderSet = S.fromList binders

    isRecordCon :: HsExpr GhcRn -> Bool
    isRecordCon RecordCon{} = True
    isRecordCon _           = False

    usesBinder (RecordCon _ _ HsRecFields{rec_flds, rec_dotdot})
      | Just (L _ dotdot) <- rec_dotdot =
          or
            [ hsVarName ident `S.member` binderSet
            | L _ HsFieldBind{hfbRHS = L _ (HsVar _ ident)} <-
                drop (unRecFieldsDotDot dotdot) rec_flds
            ]
    usesBinder _ = False

-- | The language extensions the node's syntax needs wherever it is
-- printed: a @..@ record wildcard in a construction or constructor
-- pattern (the renamer keeps the 'rec_dotdot' marker alongside the
-- fields it expanded to, so the check is purely on that marker), a
-- @\\case@ or @\\cases@ lambda, a multi-way @if@, and a view pattern.
-- These are the extension-gated syntax forms observed in real
-- definitions so far; the list errs on the side of growing.
spliceExtensions :: Data a => a -> [Extension]
spliceExtensions = nub . everything (++) ([] `mkQ` exprExts `extQ` patExts)
  where
    exprExts :: HsExpr GhcRn -> [Extension]
    exprExts (RecordCon _ _ HsRecFields{rec_dotdot})
      | isJust rec_dotdot       = [RecordWildCards]
    exprExts (HsLam _ LamCase _)  = [LambdaCase]
    exprExts (HsLam _ LamCases _) = [LambdaCase]
    exprExts HsMultiIf{}          = [MultiWayIf]
    exprExts (ExplicitTuple _ args _)
      | any isMissing args        = [TupleSections]
      where
        isMissing Missing{} = True
        isMissing _         = False
    exprExts _                    = []

    patExts :: Pat GhcRn -> [Extension]
    patExts (ConPat _ _ (RecCon HsRecFields{rec_dotdot}))
      | isJust rec_dotdot = [RecordWildCards]
    patExts ViewPat{}     = [ViewPatterns]
    patExts _             = []

-- Check whether one clause of the binding has a form we can inline.
checkClause :: Name -> LMatch GhcRn (LHsExpr GhcRn) -> ClauseDef
checkClause funName (L _ Match{m_pats, m_grhss}) =
  ClauseDef
    { clausePats = pats
    , selectable = ok
    , dispatchOk = dispatch
    , clauseRefs = bodyRefSpellings m_grhss
    , patRefs    = bodyRefSpellings pats
    }
  where
    pats    = matchPats m_pats
    binders = collectPatsBinders CollNoDictBinders pats
    -- requirements shared by both inline forms:
    dispatch =
      -- reject recursive clauses; the whole clause is spliced, so a
      -- self-reference hiding in the where clause counts too. Other
      -- clauses may still be inlined by clause selection: a site that
      -- provably selects a non-recursive clause never expands the
      -- recursive one.
      null (findVars funName m_grhss)
        -- reject clauses whose meaning depends on a forall'd type variable
        -- from the function's own signature -- inlining would capture it.
        -- the where clause travels with the body, so scan it as well
        && not (referencesTypeVar m_grhss)
    ok =
      dispatch
        -- each parameter pattern must be within the rewrite-supported set
        && all rewritablePat pats
        -- a single unguarded right-hand side
        && isJust (checkBody m_grhss)
        -- a record constructed with a wildcard ('R {..}') picks its fields
        -- up by name; a pattern binder feeding it disappears under
        -- substitution (the argument expression replaces the name), so the
        -- construction cannot survive inlining. Wildcard fields fed by
        -- where or let binders are fine: those binders travel with the body.
        && not (wildcardUsesBinder binders m_grhss)

-- | External names the body references, each paired with the spelling
-- the source uses. A name never seen through a user-written reference
-- (a record-wildcard field, say) is assumed to be spelled unqualified.
bodyRefSpellings :: Data a => a -> [(RdrName, Name)]
bodyRefSpellings body =
  S.toList . S.fromList $
    spelled <>
      [ (Unqual (nameOccName n), n)
      | n <- listify isExternalName body
      , not (n `S.member` spelledNames)
      ]
  where
    spelled      = [p | p@(_, n) <- refSpellings body, isExternalName n]
    spelledNames = S.fromList (map snd spelled)

-- Check whether this binding fits our requirements for inlining.
checkBinder :: LHsBindLR GhcRn GhcRn -> Maybe BindingDef
checkBinder b =
  case unLoc b of
    -- only allow inlining of FunBind-type bindings
    FunBind{fun_id = lFunId, fun_matches = MG{mg_alts = matches}} -> do
      let funName = unLoc lFunId
      funIdSp <- toRealSrcSpan (getLocA lFunId)
      firstAlt : _ <- Just (unLoc matches)
      let cls    = map (checkClause funName) (unLoc matches)
          arity' = length (matchPats (m_pats (unLoc firstAlt)))
          -- an arity-0 dispatch has nothing to scrutinise
          dispatchable' = arity' > 0 && all (.dispatchOk) cls
      -- a definition none of whose clauses can ever be inlined -- by
      -- clause selection or inside a spliced dispatch -- is not a
      -- candidate at all
      guard $ any (.selectable) cls || dispatchable'
      pure
        BindingDef
          { clauses      = cls
          , arity        = arity'
          , dispatchable = dispatchable'
          , funIdSpan    = funIdSp
          , neededExts   = spliceExtensions matches
          }
    _ -> Nothing

-- | Whether an argument expression definitely matches ('PatYes'),
-- definitely does not match ('PatNo'), or may or may not match
-- ('PatUnknown') a clause pattern, judged purely syntactically.
--
-- Soundness rules:
--
-- * A definite verdict must not skip a match that could diverge: both
--   'PatYes' and 'PatNo' are only answered when deciding the match
--   forces nothing beyond what the source text already exhibits -- the
--   pattern binds without looking ('VarPat'-likes), or the argument is
--   itself a literal or a constructor application, i.e. already in
--   weak head normal form at the call site.
-- * An overloaded literal matches via '(==)', so two /different/
--   literals are 'PatUnknown' -- an unlawful 'Eq' or 'fromInteger'
--   could still equate them. Equal literals are 'PatYes', assuming a
--   lawful instance, as every refactoring tool must.
-- * A 'PatYes' additionally guarantees that retrie's structural match
--   of the clause's query template succeeds at the site, so the chosen
--   clause is the one the rewrite actually splices ('rewritablePat'
--   keeps the two matchers aligned).
data PatVerdict = PatYes | PatNo | PatUnknown
  deriving stock Eq

matchPat :: LHsExpr GhcRn -> LPat GhcRn -> PatVerdict
matchPat lexpr lpat =
  case unLoc lpat of
    VarPat{}     -> PatYes
    WildPat{}    -> PatYes
    -- an irrefutable pattern always matches
    LazyPat{}    -> PatYes
    ParPat _ p   -> matchPat lexpr p
    -- the bang forces the argument before matching, but a definite
    -- verdict is only ever reached against a WHNF argument, where the
    -- bang is a no-op
    BangPat _ p  -> matchPat lexpr p
    AsPat _ _ p  -> matchPat lexpr p
    SigPat _ p _ -> matchPat lexpr p
    LitPat _ lit ->
      case unLoc expr' of
        HsLit _ lit' -> if lit == lit' then PatYes else PatNo
        _            -> PatUnknown
    NPat _ (L _ olit) mbNeg _ ->
      case overLitView expr' of
        Just (olit', negated)
          | negated == isJust mbNeg
          , ol_val olit == ol_val olit' -> PatYes
        _                               -> PatUnknown
    TuplePat _ ps Boxed ->
      case unLoc expr' of
        ExplicitTuple _ args Boxed
          | Just es <- traverse presentArg args
          , length es == length ps ->
              sequenceMatches (zipWith matchPat es ps)
        _ -> PatUnknown
    ListPat _ ps ->
      case unLoc expr' of
        ExplicitList _ es
          | length es == length ps ->
              sequenceMatches (zipWith matchPat es ps)
          -- a list literal's spine is fully exposed, so a length
          -- mismatch is definite
          | otherwise -> PatNo
        _ -> PatUnknown
    ConPat _ lcon details -> matchCon (hsVarName lcon) details
    _ -> PatUnknown
  where
    expr' = unparen lexpr

    matchCon con details
      -- a pattern synonym's match semantics are hidden behind the
      -- synonym; no verdict is possible
      | not (isDataConName con) = PatUnknown
      | Just (hd, args) <- conAppView expr' =
          if hd /= con
            -- both heads are data constructors of the argument's type,
            -- so differing heads are a definite mismatch
            then PatNo
            else case conSubPats details of
              Just ps | length ps == length args ->
                sequenceMatches (zipWith matchPat args ps)
              _ -> PatUnknown
      | otherwise = PatUnknown

    presentArg = \case
      Present _ e -> Just e
      _           -> Nothing

-- | View an expression as a saturated data-constructor application,
-- prefix or infix.
conAppView :: LHsExpr GhcRn -> Maybe (Name, [LHsExpr GhcRn])
conAppView e =
  case collectArgs (unparen e) of
    (L _ (HsVar _ ident), args)
      | let hd = hsVarName ident
      , isDataConName hd -> Just (hd, args)
    (L _ (OpApp _ lhs (L _ (HsVar _ ident)) rhs), extras)
      | let hd = hsVarName ident
      , isDataConName hd -> Just (hd, lhs : rhs : extras)
    _ -> Nothing

-- | An overloaded literal argument, together with whether it is negated.
overLitView :: LHsExpr GhcRn -> Maybe (HsOverLit GhcRn, Bool)
overLitView e =
  case unLoc (unparen e) of
    HsOverLit _ ol -> Just (ol, False)
    NegApp _ inner _
      | HsOverLit _ ol <- unLoc (unparen inner) -> Just (ol, True)
    _ -> Nothing

-- | Combine the per-parameter verdicts of one clause. Patterns match
-- left to right, so the first non-'PatYes' verdict decides: a definite
-- mismatch may only be acted on when every pattern before it definitely
-- matched -- an uncertain match to its left could diverge at runtime
-- before the mismatch is ever reached.
sequenceMatches :: [PatVerdict] -> PatVerdict
sequenceMatches vs =
  case dropWhile (== PatYes) vs of
    []    -> PatYes
    v : _ -> v

-- | Assign each call site the way it is inlined, dropping sites that
-- cannot be rewritten.
--
-- A single selectable clause of plain variable patterns matches any
-- call, so every site -- including bare references and partial
-- applications, which supply fewer arguments than the arity -- keeps
-- its default of clause 0. Otherwise clause selection needs the full
-- argument list: scanning the clauses top-down, a clause whose patterns
-- definitely mismatch is skipped; the first clause whose patterns
-- definitely match is chosen, provided it is selectable. Any
-- uncertainty -- an undecidable match, or a definitely-firing clause
-- that is guarded or otherwise unselectable -- falls back to splicing
-- the whole dispatch as a case expression ('SiteDispatch') when the
-- definition supports it, and drops the site otherwise. A bare
-- reference supplies no arguments to decide with, so it can only be
-- inlined as the dispatch (wrapped in a lambda). Backtick calls and
-- sections have no dispatch form.
selectClauseSites :: BindingDef -> [CallSite] -> [CallSite]
selectClauseSites def sites
  | [c] <- def.clauses
  , c.selectable
  , all isVarPat c.clausePats = sites
  | otherwise = mapMaybe pick sites
  where
    isVarPat p = case unLoc p of
      VarPat{} -> True
      _        -> False
    pick site
      | length site.argExprs == def.arity =
          case choose 0 def.clauses of
            Just i  -> Just site{inlineVia = SiteClause i}
            Nothing -> dispatchSite
      | null site.argExprs, def.arity > 0 = dispatchSite
      | otherwise = Nothing
      where
        dispatchSite = do
          guard (def.dispatchable && site.prefixApp)
          pure site{inlineVia = SiteDispatch}
        choose _ [] = Nothing
        choose i (c : cs) =
          case sequenceMatches (zipWith matchPat site.argExprs c.clausePats) of
            PatNo      -> choose (i + 1) cs
            PatYes     -> if c.selectable then Just i else Nothing
            PatUnknown -> Nothing

-- | External names referenced by the clauses the given sites inline;
-- feeds the import check of the target file. A dispatch site splices
-- every clause, so it references them all.
clauseRefsFor :: BindingDef -> [CallSite] -> [(RdrName, Name)]
clauseRefsFor def sites =
  S.toList . S.fromList $
    concatMap
      refsOf
      (S.toList (S.fromList (map (.inlineVia) sites)))
  where
    refsOf (SiteClause ix) = (def.clauses !! ix).clauseRefs
    -- a dispatch copies every clause's parameter patterns into its case
    -- alternatives, so their references splice along with the bodies
    refsOf SiteDispatch    =
      concatMap (\c -> c.clauseRefs <> c.patRefs) def.clauses

-- Extract identifiers and their spans from the AST.
extractNames :: HieAST a -> [(Name, [ContextInfo], RealSrcSpan)]
extractNames ast =
  map
    (\(ident, det) ->
      (ident, S.toList det.identInfo, Hie.nodeSpan ast))
    (nameIdentifiers (getSourceNodeIds ast))
  where
    nameIdentifiers identifiers =
      mapMaybe maybeName $
        M.toList identifiers
    maybeName (identifier, details) =
      case identifier of
        Left _     -> Nothing
        Right name -> Just (name, details)

isInlineSite :: [ContextInfo] -> Bool
isInlineSite = any $ \case
  Use       -> True -- site of a variable usage
  ValBind{} -> True -- site of the function definition
  _         -> False
