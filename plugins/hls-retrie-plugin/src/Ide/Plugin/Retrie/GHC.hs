{-# LANGUAGE CPP #-}
-- | Version-independent views of GHC's 'GlobalRdrEnv'.
module Ide.Plugin.Retrie.GHC
  ( lookupGRERdr
  , lookupGREName
  , lookupFieldGREs
  , greIsParentless
  , greImportModule
  , greImportQualifier
  ) where

import           Development.IDE.GHC.Compat (ImportSpec, ModuleName, Name,
                                             RdrName, gre_par, moduleName,
                                             nameOccName)
import           GHC.Data.FastString        (FastString)
import           GHC.Types.Name.Occurrence  (mkVarOccFS)
import           GHC.Types.Name.Reader      (GlobalRdrElt, GlobalRdrEnv,
                                             Parent (..), isRecFldGRE, is_as,
                                             is_decl)
import qualified GHC.Types.Name.Reader      as RdrName

#if MIN_VERSION_ghc(9,8,0)
import           GHC.Types.Name.Reader      (FieldsOrSelectors (WantField),
                                             LookupGRE (..),
                                             WhichGREs (RelevantGREsFOS, SameNameSpace))
#else
import           GHC.Types.Name.Reader      (mkRdrUnqual)
#endif

-- | Every 'GlobalRdrElt' a spelling resolves to in the scope, within
-- the spelling's own namespace.
lookupGRERdr :: GlobalRdrEnv -> RdrName -> [GlobalRdrElt]
#if MIN_VERSION_ghc(9,8,0)
lookupGRERdr env rdr = RdrName.lookupGRE env (LookupRdrName rdr SameNameSpace)
#else
lookupGRERdr env rdr = RdrName.lookupGRE_RdrName rdr env
#endif

-- | The 'GlobalRdrElt' a resolved name is in scope as, if any.
lookupGREName :: GlobalRdrEnv -> Name -> Maybe GlobalRdrElt
lookupGREName = RdrName.lookupGRE_Name

-- | Whether a name is a plain top-level binding rather than a class
-- method, record field or constructor, which live under a parent.
greIsParentless :: GlobalRdrElt -> Bool
greIsParentless gre = case gre_par gre of
  NoParent -> True
  _        -> False

-- | The record fields a label is in scope for. Fields live in their
-- own namespace from 9.8 on; the variable-namespace lookup asks for
-- them explicitly.
lookupFieldGREs :: GlobalRdrEnv -> FastString -> [GlobalRdrElt]
#if MIN_VERSION_ghc(9,8,0)
lookupFieldGREs env lbl =
  filter isRecFldGRE $
    RdrName.lookupGRE env (LookupOccName (mkVarOccFS lbl) (RelevantGREsFOS WantField))
#else
lookupFieldGREs env lbl =
  filter isRecFldGRE $ RdrName.lookupGRE_RdrName (mkRdrUnqual (mkVarOccFS lbl)) env
#endif

-- | The import-facing module an 'ImportSpec' brought a name in from
-- (@Data.Char@, not the defining @GHC.Internal.*@ module).
greImportModule :: ImportSpec -> ModuleName
#if MIN_VERSION_ghc(9,8,0)
greImportModule = moduleName . RdrName.is_mod . is_decl
#else
greImportModule = RdrName.is_mod . is_decl
#endif

-- | The qualifier an 'ImportSpec' makes a name available under (the
-- @as@ alias, or the imported module's own name without one).
greImportQualifier :: ImportSpec -> ModuleName
greImportQualifier = is_as . is_decl

