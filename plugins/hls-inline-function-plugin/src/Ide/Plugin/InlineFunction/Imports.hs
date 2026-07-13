-- | Compute the imports a target module needs so that a function body
-- spliced into it by inlining still resolves.
--
-- The body of the inlined function may reference bindings that are in
-- scope in its defining module but not at the call site -- @fromMaybe@
-- in a module that never imports "Data.Maybe", say. For each such name
-- we synthesize a minimal explicit import. Names nobody can import
-- (the defining module keeps them private) make the splice impossible,
-- and the caller is expected to leave the target file unchanged.
module Ide.Plugin.InlineFunction.Imports
  ( importEdits
  ) where

import           Data.List                   (intercalate, sort)
import qualified Data.Map                    as M
import           Data.Maybe                  (isJust, isNothing)
import qualified Data.Set                    as S
import qualified Data.Text                   as T
import           Development.IDE.GHC.Compat
import           Language.LSP.Protocol.Types (Position (..), Range (..),
                                              TextEdit (..))

-- | The import edits the target module needs so the spliced body's
-- references stay in scope, or 'Nothing' when some needed name cannot
-- be imported (its defining module does not export it), in which case
-- inlining into this module would not compile.
importEdits
  :: TcGblEnv     -- ^ The defining module: provenance of the body's references.
  -> TcGblEnv     -- ^ The target module: what is already in scope there.
  -> ParsedSource -- ^ The target module's source, for the insertion point.
  -> [Name]       -- ^ External names the spliced body references.
  -> Maybe [TextEdit]
importEdits defTc targetTc targetSource needs
  | any isNothing sources = Nothing
  | null missing = Just []
  | otherwise = Just [TextEdit (Range insertAt insertAt) importText]
  where
    -- Names the renamer inserted itself -- 'getField' behind
    -- OverloadedRecordDot, literal witnesses like 'fromInteger' -- have no
    -- 'GlobalRdrElt' in the defining module. The spliced source text never
    -- mentions them, so they need no import.
    userWritten =
      filter (isJust . lookupGRE_Name (tcg_rdr_env defTc)) $
        S.toList (S.fromList needs)
    -- names not already in scope (under any spelling) in the target
    missing =
      filter (isNothing . lookupGRE_Name (tcg_rdr_env targetTc)) userWritten
    sources = map (importModuleFor defTc) missing
    byModule =
      M.fromListWith (<>)
        [ (m, [nameOccName n])
        | (Just m, n) <- zip sources missing
        ]
    importText =
      T.pack $ unlines
        [ "import " <> moduleNameString m <> " (" <> renderOccs occs <> ")"
        | (m, occs) <- M.toAscList byModule
        ]
    insertAt = insertPosition targetSource

-- | The module to import @name@ from, or 'Nothing' if it cannot be
-- imported. A name the defining module bound locally is importable from
-- there exactly when it is exported; a name the defining module itself
-- imported is importable from whichever module that import named (which
-- handles re-exports, unlike 'nameModule'). Only plain variables and
-- type constructors get imports -- data constructors and record fields
-- need their parent in the import list, which we conservatively refuse.
importModuleFor :: TcGblEnv -> Name -> Maybe ModuleName
importModuleFor defTc name
  | not importableOcc = Nothing
  | nameIsLocalOrFrom (tcg_mod defTc) name =
      if name `elemNameSet` availsToNameSet (tcg_exports defTc)
        then Just (moduleName (tcg_mod defTc))
        else Nothing
  | Just gre <- lookupGRE_Name (tcg_rdr_env defTc) name
  , spec : _ <- gre_imp gre =
      Just (moduleName (is_mod (is_decl spec)))
  | otherwise = moduleName <$> nameModule_maybe name
  where
    occ = nameOccName name
    importableOcc = isVarOcc occ || isTcOcc occ

renderOccs :: [OccName] -> String
renderOccs = intercalate ", " . sort . map renderOcc
  where
    renderOcc occ
      | isSymOcc occ = "(" <> occNameString occ <> ")"
      | otherwise    = occNameString occ

-- | Where to insert new imports: the line after the last existing import,
-- or failing that the line after the module header, or the top of the file.
insertPosition :: ParsedSource -> Position
insertPosition (L _ m) =
  case reverse (hsmodImports m) of
    lastImport : _ | Just line <- endLine (getLocA lastImport) -> Position line 0
    _ -> case hsmodName m of
      Just lname | Just line <- endLine (getLocA lname) -> Position line 0
      _                                                 -> Position 0 0
  where
    -- 1-based end line of the span is the 0-based line after it
    endLine (RealSrcSpan sp _) = Just (fromIntegral (srcSpanEndLine sp))
    endLine _                  = Nothing
