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

import           Control.Monad                     (guard)
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

    visible = visibleIn targetEnv

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

    sources = map (importSpecFor defTc targetEnv . snd) missing
    unqualByModule =
      M.fromListWith (<>)
        [ (m, [item])
        | ((Unqual _, _), Just (m, item)) <- zip missing sources
        ]
    qualImports =
      S.fromList
        [ (m, q)
        | ((Qual q _, _), Just (m, _)) <- zip missing sources
        ]
    importText =
      T.pack $ unlines $
        [ "import " <> moduleNameString m
            <> " (" <> intercalate ", " (sort items) <> ")"
        | (m, items) <- M.toAscList unqualByModule
        ] <>
        [ "import qualified " <> moduleNameString m <> alias
        | (m, q) <- S.toAscList qualImports
        , let alias
                | q == m    = ""
                | otherwise = " as " <> moduleNameString q
        ]

-- | Everything a spelling can refer to in the given environment,
-- resolved the way the renamer resolves a written reference: qualifiers
-- are honoured and record fields participate exactly when their
-- selectors do.
visibleIn :: GlobalRdrEnv -> RdrName -> [GlobalRdrElt]
visibleIn env rdr =
  Rdr.lookupGRE env (Rdr.LookupRdrName rdr (Rdr.RelevantGREsFOS Rdr.WantNormal))

-- | The module to import @name@ from and the import-list item that
-- provides it there, or 'Nothing' if it cannot be imported.
importSpecFor :: TcGblEnv -> GlobalRdrEnv -> Name -> Maybe (ModuleName, String)
importSpecFor defTc targetEnv name =
  (,) <$> importModuleFor defTc name <*> importItemFor defTc targetEnv name

-- | The module to import @name@ from, or 'Nothing' if it cannot be
-- imported. A name the defining module bound locally is importable from
-- there exactly when it is exported; a name the defining module itself
-- imported is importable from whichever module that import named (which
-- handles re-exports, unlike 'nameModule').
importModuleFor :: TcGblEnv -> Name -> Maybe ModuleName
importModuleFor defTc name
  | nameIsLocalOrFrom (tcg_mod defTc) name =
      if name `elemNameSet` availsToNameSet (tcg_exports defTc)
        then Just (moduleName (tcg_mod defTc))
        else Nothing
  | Just gre <- lookupGRE_Name (tcg_rdr_env defTc) name
  , spec : _ <- gre_imp gre =
      Just (moduleName (is_mod (is_decl spec)))
  | otherwise = moduleName <$> nameModule_maybe name

-- | The import-list item that provides @name@: the name itself for a
-- plain variable or type constructor, and @Parent(Ctor)@ for a data
-- constructor -- an import list cannot name a constructor bare, so its
-- parent type supplies it (a constructor is only ever exported through
-- its parent, so the parent is available wherever the constructor is).
-- That item imports the parent /type/ alongside the constructor, so a
-- same-spelled type already meaning something else at the target would
-- turn its existing references ambiguous; such an item is refused.
-- Record fields would need the same parent treatment but resolve per
-- record under DuplicateRecordFields; they stay conservatively refused.
importItemFor :: TcGblEnv -> GlobalRdrEnv -> Name -> Maybe String
importItemFor defTc targetEnv name
  | isVarOcc occ || isTcOcc occ = Just (renderOcc occ)
  | isDataOcc occ = do
      gre <- lookupGRE_Name (tcg_rdr_env defTc) name
      Rdr.ParentIs parent <- Just (Rdr.gre_par gre)
      guard $ all ((== parent) . gre_name) $
        visibleIn targetEnv (Unqual (nameOccName parent))
      pure $
        renderOcc (nameOccName parent) <> "(" <> renderOcc occ <> ")"
  | otherwise = Nothing
  where
    occ = nameOccName name

renderOcc :: OccName -> String
renderOcc occ
  | isSymOcc occ = "(" <> occNameString occ <> ")"
  | otherwise    = occNameString occ
