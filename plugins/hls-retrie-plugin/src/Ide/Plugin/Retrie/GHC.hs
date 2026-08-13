{-# LANGUAGE CPP #-}
-- | Version-independent views of GHC's 'GlobalRdrEnv'.
module Ide.Plugin.Retrie.GHC
  ( lookupGRERdr
  , greImportModule
  , greImportQualifier
  , greParentOcc
  ) where

import           Development.IDE.GHC.Compat (ImportSpec, ModuleName, OccName,
                                             RdrName, gre_par, moduleName,
                                             nameOccName)
import           GHC.Types.Name.Reader      (GlobalRdrElt, GlobalRdrEnv,
                                             Parent (ParentIs), is_as, is_decl)
import qualified GHC.Types.Name.Reader      as RdrName

#if MIN_VERSION_ghc(9,8,0)
import           GHC.Types.Name.Reader      (LookupGRE (..),
                                             WhichGREs (SameNameSpace))
#endif

-- | Every 'GlobalRdrElt' a spelling resolves to in the scope, within
-- the spelling's own namespace.
lookupGRERdr :: GlobalRdrEnv -> RdrName -> [GlobalRdrElt]
#if MIN_VERSION_ghc(9,8,0)
lookupGRERdr env rdr = RdrName.lookupGRE env (LookupRdrName rdr SameNameSpace)
#else
lookupGRERdr env rdr = RdrName.lookupGRE_RdrName rdr env
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

-- | The parent type constructor of a name that can only be imported
-- via its parent (data constructors, record fields).
greParentOcc :: GlobalRdrElt -> Maybe OccName
greParentOcc gre = case gre_par gre of
  ParentIs p -> Just (nameOccName p)
  _          -> Nothing
