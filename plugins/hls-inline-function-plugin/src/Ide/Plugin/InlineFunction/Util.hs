{-# LANGUAGE CPP #-}

-- | Functions for manipulating the AST.
module Ide.Plugin.InlineFunction.Util
  ( location
  , toRealSrcSpan
  , hsVarName
  , grhsList
  , matchPats
  , refSpellings
  ) where

import           GHC.Hs

import           Data.Generics              (Data, everything, extQ, mkQ)
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

-- Every user-written reference in the tree paired with the spelling the
-- source used. GHC 9.14's renamed tree keeps the original 'RdrName'
-- beside the resolved 'Name' ('WithUserRdr'), so qualified references
-- keep their qualifier. Older GHCs drop the spelling during renaming, so
-- nothing is collected there and callers fall back to assuming
-- unqualified spellings.
#if __GLASGOW_HASKELL__ >= 913
refSpellings :: Data a => a -> [(RdrName, Name)]
refSpellings = everything (++) ([] `mkQ` varRef `extQ` exprRef `extQ` patRef)
  where
    varRef :: WithUserRdr Name -> [(RdrName, Name)]
    varRef (WithUserRdr rdr n) = [(rdr, n)]
    -- Record-field references keep their spelling in a 'FieldOcc', not
    -- a 'WithUserRdr': a bare selector use ('_message x') inside
    -- 'HsRecSelRn', and the field labels of record constructions,
    -- updates and constructor patterns. The spelling matters twice
    -- over: the selector 'Name''s own 'OccName' lives in a per-record
    -- field namespace that resolves only against that record's fields,
    -- while the written spelling resolves against every field in scope
    -- -- and a qualified label ('r { Env.f = x }') needs its qualifier
    -- at the splice point. Both are how the target re-reads the text.
    exprRef :: HsExpr GhcRn -> [(RdrName, Name)]
    exprRef (XExpr (HsRecSelRn fo)) = [fieldOccRef fo]
    exprRef (RecordCon _ _ flds)    = explicitFieldRefs flds
    exprRef (RecordUpd _ _ RegularRecUpdFields{recUpdFields}) =
      [fieldOccRef fo | L _ HsFieldBind{hfbLHS = L _ fo} <- recUpdFields]
    exprRef _                       = []

    patRef :: Pat GhcRn -> [(RdrName, Name)]
    patRef (ConPat _ _ (RecCon flds)) = explicitFieldRefs flds
    patRef _                          = []

    fieldOccRef :: FieldOcc GhcRn -> (RdrName, Name)
    fieldOccRef (FieldOcc rdr lname) = (rdr, unLoc lname)

    -- only fields the source spelled out: the implicit fields a '..'
    -- wildcard expands to have no spelling in the spliced text
    explicitFieldRefs :: HsRecFields GhcRn arg -> [(RdrName, Name)]
    explicitFieldRefs HsRecFields{rec_flds, rec_dotdot} =
      [ fieldOccRef fo
      | L _ HsFieldBind{hfbLHS = L _ fo} <- explicit
      ]
      where
        explicit = case rec_dotdot of
          Just (L _ dd) -> take (unRecFieldsDotDot dd) rec_flds
          Nothing       -> rec_flds
#else
refSpellings :: Data a => a -> [(RdrName, Name)]
refSpellings _ = []
#endif

toRealSrcSpan :: SrcSpan -> Maybe RealSrcSpan
toRealSrcSpan = \case
  RealSrcSpan sp _ -> Just sp
  _                -> Nothing

location :: LocatedA a -> Maybe RealSrcSpan
location = toRealSrcSpan . getLocA
