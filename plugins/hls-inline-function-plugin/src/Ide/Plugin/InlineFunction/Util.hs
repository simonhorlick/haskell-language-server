{-# LANGUAGE CPP #-}

-- | Functions for manipulating the AST.
module Ide.Plugin.InlineFunction.Util
  ( location
  , toRealSrcSpan
  , hsVarName
  , grhsList
  , matchPats
  ) where

import           GHC.Hs

import           Development.IDE.GHC.Compat
#if __GLASGOW_HASKELL__ >= 913
import qualified Data.List.NonEmpty         as NE
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

toRealSrcSpan :: SrcSpan -> Maybe RealSrcSpan
toRealSrcSpan = \case
  RealSrcSpan sp _ -> Just sp
  _                -> Nothing

location :: LocatedA a -> Maybe RealSrcSpan
location = toRealSrcSpan . getLocA
