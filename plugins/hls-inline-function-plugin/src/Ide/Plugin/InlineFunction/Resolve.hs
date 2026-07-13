{-# LANGUAGE CPP #-}
module Ide.Plugin.InlineFunction.Resolve
  ( nameUnderCursor
  , findDefinition
  , findAllCallSites
  , callSiteAt
  , InlineCandidate(..)
  , BindingDef(..)
  , CallSite(..)
  , CursorSite(..)
  ) where

import           Control.Monad                  (guard)
import           Data.Generics                  (Data, everything, extQ,
                                                 listify, mkQ)
import           Data.List                      (sortOn)
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
  }

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
  { params    :: ![Name]
    -- ^ Argument names. The list length determines the call-site
    -- arity to look for; the individual names are currently unused
    -- but retained for diagnostics.
  , funIdSpan :: !RealSrcSpan
    -- ^ Span of the @fun_id@ (the function name at its binding site). This
    -- can differ from 'nameSrcSpan' of the bound 'Name' -- for example, a
    -- type class default method's 'Name' is declared by the type signature
    -- in the class header, but the @fun_id@ of the default-method FunBind
    -- is at the implementation site.
  , bodyRefs  :: ![(RdrName, Name)]
    -- ^ External names the body (including its where clause) references,
    -- each paired with the spelling the source text uses -- the spliced
    -- code keeps that spelling, so a qualified reference needs its
    -- qualifier in scope at the splice point. Parameters and body-local
    -- binders are internal names, so they are excluded automatically.
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
          | hsVarName ident == name -> do
              appSp  <- location candidate
              headSp <- location op
              lhsSp  <- location lhs
              rhsSp  <- location rhs
              pure CallSite { application = appSp, headRef = headSp, arguments = [lhsSp, rhsSp] }
        -- an operator section -- @(`name` e)@ or @(e `name`)@ -- supplies
        -- one argument; retrie's section rewrites eta-expand the
        -- remaining parameters, so the section inlines to a lambda
        -- spliced inside the source parens.
        _ | Just (op, operand) <- sectionParts candidate
          , L _ (HsVar _ ident) <- op
          , hsVarName ident == name
          , arity >= 2 -> do
              appSp  <- location candidate
              headSp <- location op
              operSp <- location operand
              pure CallSite { application = appSp, headRef = headSp, arguments = [operSp] }
        _ -> case collectArgs candidate of
          -- a fully-applied call, or a bare 'HsVar' reference (zero args)
          -- that retrie will eta-expand.
          (hd@(L _ (HsVar _ ident)), args)
            | hsVarName ident == name
            , length args == arity || null args -> do
                appSp  <- location candidate
                headSp <- location hd
                argSps <- traverse location args
                pure CallSite { application = appSp, headRef = headSp, arguments = argSps }
          -- an infix call that also supplies extra arguments:
          -- @(1 `e` 2) 3@ passes the first two arguments through the
          -- operator and the rest by ordinary application
          (headExpr, extras)
            | L _ (OpApp _ lhs op@(L _ (HsVar _ ident)) rhs) <- unparen headExpr
            , hsVarName ident == name
            , not (null extras)
            , 2 + length extras == arity -> do
                appSp  <- location candidate
                headSp <- location op
                argSps <- traverse location (lhs : rhs : extras)
                pure CallSite { application = appSp, headRef = headSp, arguments = argSps }
          _ -> Nothing

    unparen :: LHsExpr GhcRn -> LHsExpr GhcRn
    unparen (L _ (HsPar _ inner)) = unparen inner
    unparen le                    = le

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

-- Allow only simple variable patterns
checkPattern :: LPat GhcRn -> Maybe Name
checkPattern pat =
  case unLoc pat of
    VarPat _ (L _ n) -> Just n
    _                -> Nothing

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

-- Check whether this pattern binding has an appropriate form.
checkMatch
  :: Name
  -> RealSrcSpan
  -> LMatch GhcRn (LHsExpr GhcRn)
  -> Maybe BindingDef
checkMatch funName funIdSp (L _ Match{m_pats, m_grhss}) = do
  -- check each parameter has an appropriate form
  params <- traverse checkPattern (matchPats m_pats)
  _ <- checkBody m_grhss
  -- reject recursive bindings; the whole match is inlined, so a
  -- self-reference hiding in the where clause counts too
  guard $ null (findVars funName m_grhss)
  -- reject matches whose meaning depends on a forall'd type variable
  -- from the function's own signature -- inlining would capture it.
  -- the where clause travels with the body, so scan it as well
  guard $ not (referencesTypeVar m_grhss)
  pure $
    BindingDef
      { params    = params
      , funIdSpan = funIdSp
      , bodyRefs  = bodyRefSpellings m_grhss
      }

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
    -- only allow inlining of FunBind-type bindings. extract the matches and
    -- check there is only one.
    FunBind{fun_id = lFunId, fun_matches = MG{mg_alts = matches}} -> do
      let funName = unLoc lFunId
      funIdSp <- toRealSrcSpan (getLocA lFunId)
      case unLoc matches of
        -- ensure we have a single match
        [match] -> checkMatch funName funIdSp match
        _       -> Nothing
    _ -> Nothing

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
