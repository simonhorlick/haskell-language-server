-- | Compute the imports a target module needs so that a function body
-- spliced into it by inlining still resolves.
--
-- The spliced text keeps the defining module's spellings, so each of the
-- body's external references is checked as spelled: a bare reference
-- needs its name in scope unqualified, a qualified reference needs an
-- import under that qualifier. For spellings that resolve to nothing we
-- synthesize an import providing them. Names nobody can import (the
-- defining module keeps them private) and spellings that already mean
-- something else at the target make the splice impossible, and the
-- caller is expected to leave the target file unchanged.
module Ide.Plugin.InlineFunction.Imports
  ( importEdits
  ) where

import           Data.List                         (intercalate, sort)
import qualified Data.Map                          as M
import           Data.Maybe                        (isJust, isNothing)
import qualified Data.Set                          as S
import qualified Data.Text                         as T
import           Development.IDE.GHC.Compat
import           Development.IDE.Plugin.CodeAction (newImportInsertRange)
import qualified GHC.Types.Name.Reader             as Rdr
import           Language.LSP.Protocol.Types       (TextEdit (..))

-- | The import edits the target module needs so the spliced body's
-- references stay in scope, or 'Nothing' when some needed spelling
-- cannot be provided (its defining module does not export the name, or
-- the spelling already means something else here), in which case
-- inlining into this module would not compile.
importEdits
  :: TcGblEnv     -- ^ The defining module: provenance of the body's references.
  -> TcGblEnv     -- ^ The target module: what is already in scope there.
  -> ParsedSource -- ^ The target module's source, for the insertion point.
  -> T.Text       -- ^ The target module's text, for the insertion point.
  -> [(RdrName, Name)]
  -- ^ External names the spliced body references, with their spellings.
  -> Maybe [TextEdit]
importEdits defTc targetTc targetSource targetContents needs
  | any conflicting userWritten = Nothing
  | any isNothing sources = Nothing
  | null missing = Just []
  | otherwise = do
      (range, _indent) <- newImportInsertRange targetSource targetContents
      pure [TextEdit range importText]
  where
    targetEnv = tcg_rdr_env targetTc

    -- Names the renamer inserted itself -- 'getField' behind
    -- OverloadedRecordDot, literal witnesses like 'fromInteger' -- have no
    -- 'GlobalRdrElt' in the defining module. The spliced source text never
    -- mentions them, so they need no import.
    userWritten =
      S.toList . S.fromList $
        filter (isJust . lookupGRE_Name (tcg_rdr_env defTc) . snd) needs

    -- Everything the spelling can refer to at the target, resolved the
    -- way the renamer resolves a written reference: qualifiers are
    -- honoured and record fields participate exactly when their
    -- selectors do.
    visible rdr =
      Rdr.lookupGRE targetEnv (Rdr.LookupRdrName rdr (Rdr.RelevantGREsFOS Rdr.WantNormal))

    -- spellings that do not already mean the right thing in the target
    missing = filter (not . satisfied) userWritten
    satisfied (rdr, n) = any ((== n) . gre_name) (visible rdr)

    -- the spelling also names something /else/ at the target: the
    -- spliced reference would resolve to the wrong thing or be
    -- ambiguous, and an added import could never repair that, so
    -- inlining here is refused. This covers spellings the import edit
    -- would provide as well as ones already in scope: a satisfied
    -- spelling with a second candidate is exactly GHC's "ambiguous
    -- occurrence" error.
    conflicting (rdr, n) = any ((/= n) . gre_name) (visible rdr)

    sources = map (importModuleFor defTc . snd) missing
    unqualByModule =
      M.fromListWith (<>)
        [ (m, [nameOccName n])
        | ((Unqual _, n), Just m) <- zip missing sources
        ]
    qualImports =
      S.fromList
        [ (m, q)
        | ((Qual q _, _), Just m) <- zip missing sources
        ]
    importText =
      T.pack $ unlines $
        [ "import " <> moduleNameString m <> " (" <> renderOccs occs <> ")"
        | (m, occs) <- M.toAscList unqualByModule
        ] <>
        [ "import qualified " <> moduleNameString m <> alias
        | (m, q) <- S.toAscList qualImports
        , let alias
                | q == m    = ""
                | otherwise = " as " <> moduleNameString q
        ]

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
