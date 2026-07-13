{-# LANGUAGE CPP #-}
module Ide.Plugin.InlineFunction.Resolve
  ( findInlineCandidate
  , InlineCandidate(..)
  , BindingDef(..)
  , CallSite(..)
  ) where

import           Control.Monad                  (guard)
import           Data.Generics                  (listify)
import qualified Data.Map                       as M
import           Data.Maybe                     (listToMaybe, mapMaybe)
import qualified Data.Set                       as S
import           Development.IDE.Core.RuleTypes (HieAstResult (..))
import           Development.IDE.GHC.Compat
import           Development.IDE.Spans.AtPoint  (pointCommand)
import           GHC.Iface.Ext.Types            (ContextInfo (..), HieAST,
                                                 IdentifierDetails (identInfo))
import qualified GHC.Iface.Ext.Types            as Hie
import           Ide.Plugin.InlineFunction.Util (grhsList, hsVarName, location,
                                                 matchPats, toRealSrcSpan)
import           Language.LSP.Protocol.Types    (Position)

-- | A fully-applied call to the function: span of the entire applied
-- expression, plus argument spans (in source order).
data CallSite = CallSite
  { application :: !RealSrcSpan
    -- ^ Span of the function.
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
  }

-- | Check the AST for what's currently under the cursor. If it's possible to
-- inline it return an 'InlineCandidate'.
--
-- Note: This function should be fast as it is called frequently.
findInlineCandidate
  :: HieAstResult
  -> RenamedSource
  -> Position
  -> Maybe InlineCandidate
findInlineCandidate HAR{hieAst} rn pos = do
  -- extract the identifiers under the cursor
  let names = concat $ pointCommand hieAst pos extractNames
  -- restrict to identifiers at valid inline sites
  (name, _, _) <- listToMaybe $ filter
    (\(n, ctxs, _) ->
      isInlineSite ctxs &&
      -- omit identifiers that are types
      not (isTyConName n || isTyVarName n))
    names
  binder <- findBinder rn name
  -- check whether this binder is appropriate for inlining
  definition <- checkBinder binder
  let
    arity = length definition.params
    sites = findAllCallSites rn name arity
  -- omit the candidate entirely when there is nothing to rewrite
  guard $ not (null sites)
  pure $
    InlineCandidate
      { name       = name
      , definition = definition
      , sites      = sites
      }

-- | Find all call sites of @name@ in the renamed source.
--
-- A call site is either fully applied (@length args == arity@) or a bare
-- @HsVar@ reference with no arguments (@null args@). The latter covers both
-- arity-0 definitions (where the bare reference /is/ the call) and partial
-- references like @map double [1, 2, 3]@ for a 1-ary @double@. Retrie's
-- rewrite construction emits an arity-0 rewrite alongside the fully-applied
-- one, so a bare reference inlines as an eta-expanded lambda.
findAllCallSites :: RenamedSource -> Name -> Int -> [CallSite]
findAllCallSites rn name arity = mapMaybe toCallSite $ listify isCandidate rn
  where
    -- an 'OpApp' node allows for infix calls like @1 `add` 2@. every 'HsVar'
    -- of @name@ is a candidate -- 'toCallSite' decides whether it stands
    -- alone (a partial reference) or sits inside a larger application we
    -- already match via 'HsApp'.
    isCandidate node =
      case unLoc node of
        HsApp{}       -> arity > 0
        OpApp{}       -> arity == 2
        HsVar _ ident -> hsVarName ident == name
        _             -> False
    toCallSite candidate =
      case unLoc candidate of
        -- backtick infix application: lhs `name` rhs
        OpApp _ lhs (L _ (HsVar _ ident)) rhs
          | hsVarName ident == name -> do
              appSp <- location candidate
              lhsSp <- location lhs
              rhsSp <- location rhs
              pure CallSite { application = appSp, arguments = [lhsSp, rhsSp] }
        _ -> case collectArgs candidate of
          -- a fully-applied call, or a bare 'HsVar' reference (zero args)
          -- that retrie will eta-expand.
          (L _ (HsVar _ ident), args)
            | hsVarName ident == name
            , length args == arity || null args -> do
                appSp  <- location candidate
                argSps <- traverse location args
                pure CallSite { application = appSp, arguments = argSps }
          _ -> Nothing

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

-- Finds uses of 'p' in 'body'
findVars :: Name -> LHsExpr GhcRn -> [RealSrcSpan]
findVars p body =
  mapMaybe location $
    listify (isVarNamed p) body

-- | True when the body references a type variable -- a 'HsTyVar'
-- whose 'Name' is itself a type variable (rather than a type
-- constructor). The only way a type variable can be referenced from
-- an expression body is via a forall'd type variable brought into
-- scope by 'ScopedTypeVariables' (or the implicit forall in the
-- function's own type signature). Inlining the body at a call site
-- where the corresponding forall'd variable is bound to something
-- else would change its meaning, so we refuse to offer the action.
bodyReferencesTypeVar :: LHsExpr GhcRn -> Bool
bodyReferencesTypeVar body =
  any isTyVarRef $ listify (\(_ :: HsType GhcRn) -> True) body
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
  body   <- checkBody m_grhss
  -- reject recursive bindings
  guard $ null (findVars funName body)
  -- reject bodies whose meaning depends on a forall'd type variable
  -- from the function's own signature -- inlining would capture it
  guard $ not (bodyReferencesTypeVar body)
  pure $
    BindingDef
      { params    = params
      , funIdSpan = funIdSp
      }

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
