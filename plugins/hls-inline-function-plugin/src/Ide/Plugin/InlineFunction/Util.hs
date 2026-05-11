{-# LANGUAGE CPP #-}

-- | Functions for manipulating the AST.
module Ide.Plugin.InlineFunction.Util
  ( addParens
  , maybeParenthesize
  , replaceExpr
  , replaceAll
  , locateSpan
  , location
  , toRealSrcSpan
  , hsVarName
  , grhsList
  , matchPats
  ) where

import           GHC.Hs
import           Language.Haskell.GHC.ExactPrint.Transform

import           Data.Generics                             (listify)
import           Data.List                                 (find)
import qualified Data.Map                                  as M
import           Data.Maybe                                (listToMaybe,
                                                            mapMaybe)
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.ExactPrint            (epl)
#if !MIN_VERSION_ghc(9,10,0)
import           GHC.Hs                                    (HsToken (..))
import           GHC.Parser.Annotation                     (TokenLocation (..))
#endif
#if __GLASGOW_HASKELL__ >= 913
import qualified Data.List.NonEmpty                        as NE
#endif

-- Extract the 'Name' from the @LIdP@ / @LIdOccP@ wrapper inside an 'HsVar'.
-- GHC 9.14 changed 'HsVar' to carry a 'WithUserRdr' instead of a bare 'Name'.
#if __GLASGOW_HASKELL__ >= 913
hsVarName :: GenLocated l (WithUserRdr Name) -> Name
hsVarName = unLocWithUserRdr
#else
hsVarName :: GenLocated l Name -> Name
hsVarName = unLoc
#endif

-- Convert 'grhssGRHSs' to a plain list. GHC 9.14 changed its type from
-- @[LGRHS p body]@ to @NonEmpty (LGRHS p body)@.
#if __GLASGOW_HASKELL__ >= 913
grhsList :: NE.NonEmpty a -> [a]
grhsList = NE.toList
#else
grhsList :: [a] -> [a]
grhsList = id
#endif

-- Unwrap the 'm_pats' field of a 'Match'. GHC 9.12 wrapped that field in a
-- located @XRec@; older GHC keeps it as a plain list.
#if MIN_VERSION_ghc(9,12,0)
matchPats :: GenLocated l a -> a
matchPats = unLoc
#else
matchPats :: a -> a
matchPats = id
#endif

-- If necessary, wrap with parenthesis.
maybeParenthesize
  :: RealSrcSpan
  -> ParsedSource
  -> LHsExpr GhcPs
  -> LHsExpr GhcPs
maybeParenthesize span ps expression =
  if contextNeedsParens span ps then
    addParens expression
  else
    expression

-- | Replace the expression at @target@ with @replacement@.
replaceExpr
  :: RealSrcSpan
  -> LHsExpr GhcPs
  -> LHsExpr GhcPs
  -> LHsExpr GhcPs
replaceExpr target replacement original =
  replaceAll (M.singleton target replacement) original

-- | Replace all of the expressions with matching spans with their
-- substitutions.
replaceAll
  :: M.Map RealSrcSpan (LHsExpr GhcPs)
  -> LHsExpr GhcPs
  -> LHsExpr GhcPs
replaceAll substitutions original =
  case location original of
    Just span ->
      case M.lookup span substitutions of
        -- ensure the leading spaces are carried over to the substitution
        Just substitution -> setEntryDP substitution (getEntryDP original)
        Nothing           -> original
    Nothing -> original

-- | Locate the (first) expression that has exactly this span.
locateSpan :: RealSrcSpan -> ParsedSource -> Maybe (LHsExpr GhcPs)
locateSpan target ps =
  listToMaybe $
    listify (\expr -> location expr == Just target) ps

toRealSrcSpan :: SrcSpan -> Maybe RealSrcSpan
toRealSrcSpan = \case
  RealSrcSpan sp _ -> Just sp
  _                -> Nothing

location :: LocatedA a -> Maybe RealSrcSpan
location = toRealSrcSpan . getLocA

-- | Whether the expression at @target@ sits in a context that binds more
-- tightly than the inlined body, so the body must be parenthesized.
contextNeedsParens :: RealSrcSpan -> ParsedSource -> Bool
contextNeedsParens target ps =
  maybe False childNeedsParens (parentExpr target ps)

-- | The smallest expression that strictly contains @target@, i.e. the nearest
-- enclosing expression.
parentExpr :: RealSrcSpan -> ParsedSource -> Maybe (HsExpr GhcPs)
parentExpr target ps =
  fmap snd (find nestedInAll enclosing)
  where
    -- every expression that strictly contains @target@, paired with its span
    enclosing = mapMaybe enclosingExpr (listify (const True) ps)

    enclosingExpr :: LHsExpr GhcPs -> Maybe (RealSrcSpan, HsExpr GhcPs)
    enclosingExpr expr =
      case location expr of
        Just rsp
          | spanContains rsp target && rsp /= target -> Just (rsp, unLoc expr)
        _                                            -> Nothing

    -- the innermost enclosing expression is the one nested inside all of them
    nestedInAll (sp, _) =
      all (\(outer, _) -> spanContains outer sp) enclosing

-- | Whether @outer@ fully contains @inner@; sharing an endpoint is allowed.
spanContains :: RealSrcSpan -> RealSrcSpan -> Bool
spanContains outer inner =
     realSrcSpanStart outer <= realSrcSpanStart inner
  && realSrcSpanEnd inner <= realSrcSpanEnd outer

-- | Whether this expression forces its immediate children to be parenthesized.
-- Mirrors the parenthesization logic in @Development.IDE.GHC.ExactPrint@.
childNeedsParens :: HsExpr GhcPs -> Bool
childNeedsParens HsLam{}         = False
#if !MIN_VERSION_ghc(9,9,0)
childNeedsParens HsLamCase{}     = False
#endif
childNeedsParens HsApp{}         = True
childNeedsParens HsAppType{}     = True
childNeedsParens OpApp{}         = True
childNeedsParens HsPar{}         = False
childNeedsParens SectionL{}      = False
childNeedsParens SectionR{}      = False
childNeedsParens ExplicitTuple{} = False
childNeedsParens ExplicitSum{}   = False
childNeedsParens HsCase{}        = False
childNeedsParens HsIf{}          = False
childNeedsParens HsMultiIf{}     = False
childNeedsParens HsLet{}         = False
childNeedsParens HsDo{}          = False
childNeedsParens ExplicitList{}  = False
childNeedsParens RecordCon{}     = False
childNeedsParens RecordUpd{}     = True
childNeedsParens _               = True

-- | Wrap an expression in parentheses, if it needs them, with parentheses that
-- exactprint will actually render.
--
-- 'parenthesizeHsExpr' produces an 'HsPar' whose parenthesis tokens carry no
-- source location, so on their own they print as nothing. Give them a
-- location and pull the wrapped expression flush against the opening paren.
--
-- GHC 9.10 changed 'HsPar' to carry its parenthesis tokens as an 'EpToken'
-- pair; older GHC keeps them in separate 'LHsToken' fields.
addParens :: LHsExpr GhcPs -> LHsExpr GhcPs
addParens expr =
  case parenthesizeHsExpr appPrec expr of
#if MIN_VERSION_ghc(9,10,0)
    L l (HsPar _ inner) ->
      L l (HsPar (EpTok (epl 0), EpTok (epl 0)) (setEntryDP inner (SameLine 0)))
#else
    L l (HsPar x _ inner _) ->
      L l (HsPar x
                 (L (TokenLoc (epl 0)) HsTok)
                 (setEntryDP inner (SameLine 0))
                 (L (TokenLoc (epl 0)) HsTok))
#endif
    notParenthesized -> notParenthesized
