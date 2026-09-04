{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PatternSynonyms     #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Import resolution for inlined definitions.
--
-- Inlining splices the defining module's text into the target, so the
-- names free in that text must still refer to what they referred to in
-- the defining module. Per template occurrence, against the target
-- module's scope:
--
--   1. a name in scope in the target under the template's spelling
--      splices verbatim;
--   2. a name in scope only under other spellings is respelled to the
--      first spelling that resolves to exactly that name — a contested
--      spelling would splice as an ambiguous occurrence — and the
--      match refuses when every in-scope spelling is contested;
--   3. a name not in scope keeps its spelling and queues an import
--      from its provenance module — unless the spelling is already
--      bound to a /different/ name in the target, or its qualifier
--      already aliases a /different/ module there; either refuses the
--      match (we neither invent import aliases nor overload existing
--      ones);
--   4. a name the defining module does not export refuses the match.
--
-- A refusal has the same semantics as a capture refusal: the
-- transformer returns 'NoMatch' and the call site is left untouched.
module Ide.Plugin.Retrie.Imports
  ( DefScope
  , mkDefScope
  , TargetScope (..)
  , mkTargetScope
  , requalifyRewrite
  , ImportForm (..)
  , renderImport
  ) where

import           Control.Monad.Trans.Class                 (lift)
import           Control.Monad.Trans.State.Strict          (StateT, modify',
                                                            runStateT)
import           Data.Data                                 (Data)
import           Data.List                                 (nub, sortOn)
import           Data.List.NonEmpty                        (NonEmpty (..))
import           Data.Map.Strict                           (Map)
import qualified Data.Map.Strict                           as Map
import           Data.Set                                  (Set)
import qualified Data.Set                                  as Set

import           Development.IDE                           (TcModuleResult (..))
import           Development.IDE.GHC.Compat                (GenLocated (L),
                                                            GhcPs, ModuleName,
                                                            Name, OccName,
                                                            RdrName (..),
                                                            availsToNameSet,
                                                            getLocA, gre_imp,
                                                            gre_lcl, gre_name,
                                                            moduleName,
                                                            moduleNameString,
                                                            nameOccName,
                                                            occNameString,
                                                            pattern RealSrcSpan,
                                                            rdrNameOcc)
import qualified Development.IDE.GHC.Compat                as GHC
import           Development.IDE.GHC.ExactPrint.Annotation (withCommas)
import           Development.IDE.GHC.ExactPrint.IE         (ieVar, mkIEName,
                                                            mkTypeWithIE)
import qualified GHC                                       as GHCGHC
import           GHC.Data.EnumSet                          (EnumSet)
import           GHC.Data.FastString                       (FastString,
                                                            unpackFS)
import           GHC.Hs.ImpExp                             (EpAnnImportDecl (..),
                                                            ImportDecl (..),
                                                            ImportDeclQualifiedStyle (..),
                                                            ImportListInterpretation (..),
                                                            LIE, LImportDecl,
                                                            XImportDeclPass (..),
                                                            simpleImportDecl)
import           GHC.Parser.Annotation                     (AnnList (..),
                                                            AnnListBrackets (..),
                                                            DeltaPos (..),
                                                            EpAnn (..),
                                                            EpToken (..),
                                                            LocatedLI,
                                                            emptyComments,
                                                            noAnn)
import           GHC.Types.Name                            (isBuiltInSyntax,
                                                            isInternalName)
import           GHC.Types.Name.Reader                     (GlobalRdrEnv,
                                                            globalRdrEnvElts,
                                                            lookupGRE_Name,
                                                            mkRdrQual,
                                                            mkRdrUnqual,
                                                            unQualOK)
import           GHC.Types.Name.Set                        (NameSet,
                                                            elemNameSet)
import           GHC.Types.SourceText                      (SourceText (NoSourceText))
import           Language.Haskell.Syntax.Basic             (FieldLabelString (..))

import           Retrie                                    (RenameInfo, astA)
import           Retrie.ExactPrint                         (AnnotatedImports,
                                                            d0, dn,
                                                            noAnnSrcSpanDP0,
                                                            noAnnSrcSpanDP1,
                                                            seedA, setEntryDP,
                                                            unsafeMkA)
import           Retrie.RenameInfo                         (riNameMap)
import           Retrie.SYB                                (everywhereM, extM,
                                                            mkM)
import           Retrie.Types                              (MatchResult (..),
                                                            Rewrite,
                                                            Template (..))
import           Retrie.Universe                           (Universe)

import           Ide.Plugin.Retrie.GHC
import           Ide.Plugin.Retrie.Transformer

-- ---------------------------------------------------------------------
-- Scope views

-- | What the defining module knows: how to resolve the template's
-- occurrence spans to 'Name's, and where each name can be imported
-- from.
data DefScope = DefScope
  { dsModule    :: ModuleName
  , dsNames     :: Map GHCGHC.RealSrcSpan Name
  , dsFiles     :: Set FastString
  , dsGlobalEnv :: GlobalRdrEnv
  , dsExports   :: NameSet
  }

mkDefScope :: RenameInfo -> TcModuleResult -> DefScope
mkDefScope ri tmr = DefScope
  { dsModule = moduleName (GHC.tcg_mod tc)
  , dsNames = names
  , dsFiles = Set.fromList (map GHC.srcSpanFile (Map.keys names))
  , dsGlobalEnv = GHC.tcg_rdr_env tc
  , dsExports = availsToNameSet (GHC.tcg_exports tc)
  }
  where
    tc = tmrTypechecked tmr
    names = riNameMap ri

-- | What the target module knows: which names are in scope under which
-- spellings, and which modules its import qualifiers are bound to.
data TargetScope = TargetScope
  { tsGlobalEnv  :: GlobalRdrEnv
  , tsAliases    :: Map ModuleName [ModuleName]
  , tsExtensions :: EnumSet GHC.Extension
  }

mkTargetScope :: TcModuleResult -> TargetScope
mkTargetScope tmr = TargetScope
  { tsGlobalEnv = env
  , tsExtensions =
      GHC.extensionFlags (GHCGHC.ms_hspp_opts (GHCGHC.pm_mod_summary (tmrParsed tmr)))
  , tsAliases =
      Map.fromListWith (++)
        [ (greImportQualifier is, [greImportModule is])
        | gre <- globalRdrEnvElts env
        , is <- gre_imp gre
        ]
  }
  where
    env = GHC.tcg_rdr_env (tmrTypechecked tmr)

--------------------------------------------------------------------------------

-- | Wrap a rewrite's 'MatchResultTransformer' with import resolution.
requalifyRewrite
  :: (String -> IO ())
  -- ^ Refusal logger.
  -> DefScope
  -> TargetScope
  -> Rewrite Universe
  -> Rewrite Universe
requalifyRewrite logRefuse def tgt =
  overTransformer $ \orig ctxt match -> orig ctxt match >>= post
  where
    post NoMatch = pure NoMatch
    post (MatchResult sub tmpl) =
      case requalifyTemplate def tgt (astA (tTemplate tmpl)) of
        Left reason -> do
          logRefuse reason
          pure NoMatch
        Right (ast', items) ->
          pure $ MatchResult sub tmpl
            { tTemplate = unsafeMkA ast' (seedA (tTemplate tmpl))
            , tImports = tImports tmpl <> renderImports (consolidate items)
            }

-- ---------------------------------------------------------------------
-- Requalification

-- | One import requirement discovered while requalifying a template.
data ImportItem
  = ItemQualAs ModuleName
    -- ^ The occurrence keeps its qualified source spelling; import
    -- the module under that alias.
  | ItemMember OccName (Maybe OccName)
    -- ^ The occurrence keeps its unqualified source spelling; import
    -- it explicitly (via its parent type constructor, when present).
  deriving (Eq)

type RequalM = StateT [(ModuleName, ImportItem)] (Either String)

-- | Resolve every located 'RdrName' the template carries against the target
-- module's scope. Template-local names (quantifiers, let\/lambda binders)
-- resolve to internal 'Name's and are left alone ('Retrie.Subst.subst'
-- replaces the quantifiers right after this pass runs).
requalifyTemplate
  :: Data ast
  => DefScope
  -> TargetScope
  -> ast
  -> Either String (ast, [(ModuleName, ImportItem)])
requalifyTemplate def tgt ast =
  runStateT (everywhereM (mkM visit `extM` dotField) ast) []
  where
    visit :: GHCGHC.LocatedN RdrName -> RequalM (GHCGHC.LocatedN RdrName)
    visit lrdr@(L l rdr) = case getLocA lrdr of
      RealSrcSpan sp _
        | GHC.srcSpanFile sp `Set.member` dsFiles def ->
            case Map.lookup sp (dsNames def) of
              Nothing ->
                refuse $ "unresolvable occurrence " ++ showRdr rdr
              Just name
                -- Built-in syntax ([], (:), tuples) is always in
                -- scope and never importable; no 'GlobalRdrEnv' has
                -- an entry for it, so it must splice verbatim.
                | isInternalName name || isBuiltInSyntax name -> pure lrdr
                | otherwise -> L l <$> resolveExternal rdr name
      _ -> pure lrdr

    -- An OverloadedRecordDot label is a 'FieldLabelString', not an
    -- 'RdrName': the renamer resolves no 'Name' for it, and HasField
    -- is only solved when the field's selector is in scope at the use
    -- site. Which record the label picked is a typing matter, so every
    -- field the defining module has in scope under the label must be
    -- in scope in the target.
    dotField :: GHCGHC.DotFieldOcc GhcPs -> RequalM (GHCGHC.DotFieldOcc GhcPs)
    dotField dfo@GHCGHC.DotFieldOcc{dfoLabel = L _ (FieldLabelString lbl)}
      | null defFields =
          refuse $ "no record field " ++ unpackFS lbl ++ " in scope in the defining module"
      | all (`elem` tgtFields) defFields = pure dfo
      | otherwise =
          refuse $ "record field " ++ unpackFS lbl ++ " is not in scope in the target"
      where
        defFields = map gre_name (lookupFieldGREs (dsGlobalEnv def) lbl)
        tgtFields = map gre_name (lookupFieldGREs (tsGlobalEnv tgt) lbl)

    resolveExternal rdr name
      | s : _ <- filter unambiguous candidates = pure s
      | not (null spellings) =
          refuse $ "every in-scope spelling of " ++ showRdr rdr
            ++ " is ambiguous in the target"
      | otherwise = importOrRefuse rdr name
      where
        spellings = spellingsInScope tgt name
        -- Prefer the template's own spelling when it is in scope for
        -- our name, then 'spellingsInScope''s preference order
        -- (unqualified first, short qualifiers next).
        candidates
          | rdr `elem` spellings = rdr : filter (/= rdr) spellings
          | otherwise = spellings
        -- A spelling can be in scope for our name and still be
        -- contested: another import or a local definition also
        -- provides the occurrence string, and GHC rejects the
        -- occurrence as ambiguous. Only a spelling resolving to
        -- exactly our name may be spliced.
        unambiguous s = case occupiedBy tgt s of
          [] -> False
          ns -> all (== name) ns

    importOrRefuse rdr name = case provenanceOf def name of
      Just Importable{..}
        | not (null (occupiedBy tgt rdr)) ->
            refuse $ "spelling " ++ showRdr rdr
              ++ " is already bound to a different name in the target"
        | Qual q _ <- rdr ->
            -- A new @import qualified M as Q@ rebinds the whole @Q.*@
            -- namespace, not just this occurrence: any pre-existing
            -- @Q.x@ where both modules export @x@ would become
            -- ambiguous. Refuse unless the qualifier is free or
            -- already bound to the module we need (widening the same
            -- module's alias cannot introduce ambiguity).
            case Map.findWithDefault [] q (tsAliases tgt) of
              ms | any (/= pModule) ms ->
                    refuse $ "qualifier " ++ moduleNameString q
                      ++ " already aliases a different module in the target"
                 | otherwise -> do
                    add (pModule, ItemQualAs q)
                    pure rdr
        | Unqual occ <- rdr -> do
            add (pModule, ItemMember occ pParent)
            pure rdr
        | otherwise ->
            refuse $ "unsupported spelling " ++ showRdr rdr
      Just (LocalOnly m) ->
        refuse $ showRdr rdr ++ " is not exported from "
          ++ moduleNameString m
      Nothing ->
        refuse $ "no provenance for " ++ showRdr rdr

    add item = modify' (++ [item])

    refuse :: String -> RequalM a
    refuse = lift . Left

    showRdr = occNameString . rdrNameOcc

-- ---------------------------------------------------------------------
-- Scope queries

-- | Where a name a template references can come from, in a module that
-- does not already have it in scope.
data Provenance
  = Importable
      { pModule :: ModuleName
        -- ^ The module to import the name from: the import-facing
        -- module the defining module itself used (so a re-exported
        -- name imports as @Data.Char@, not its defining
        -- @GHC.Internal.*@ module), or the defining module itself for
        -- its own exported definitions.
      , pParent :: Maybe OccName
        -- ^ For names importable only via their parent (data
        -- constructors, record fields): the parent type constructor,
        -- yielding the @import M (T (C))@ form.
      }
  | LocalOnly ModuleName
    -- ^ Defined in the defining module and not exported: referencable
    -- only inside that module.

-- | Classify a 'Name' against the defining module's scope. 'Nothing'
-- when that scope somehow does not know the name — callers must not
-- splice a reference they cannot classify. When several imports
-- provide the name, the import-facing module is chosen
-- deterministically (alphabetically first).
provenanceOf :: DefScope -> Name -> Maybe Provenance
provenanceOf DefScope{..} name = do
  gre <- lookupGRE_Name dsGlobalEnv name
  if gre_lcl gre
    then Just $
      if name `elemNameSet` dsExports
        then Importable dsModule (greParentOcc gre)
        else LocalOnly dsModule
    else case sortOn moduleNameString (map greImportModule (gre_imp gre)) of
      m : _ -> Just $ Importable m (greParentOcc gre)
      []    -> Nothing

-- | Every spelling under which a 'Name' is in scope in the target, in
-- preference order: unqualified first (when available), then qualified
-- spellings by ascending qualifier length. Empty when the name is not
-- in scope there.
spellingsInScope :: TargetScope -> Name -> [RdrName]
spellingsInScope TargetScope{tsGlobalEnv} name =
  case lookupGRE_Name tsGlobalEnv name of
    Nothing -> []
    Just gre ->
      [ mkRdrUnqual occ | unQualOK gre ]
        ++ [ mkRdrQual q occ | q <- quals gre ]
  where
    occ = nameOccName name
    quals gre =
      nub $
        sortOn (\q -> (length (moduleNameString q), moduleNameString q)) $
          map greImportQualifier (gre_imp gre)

-- | The 'Name's a spelling resolves to in the target's scope (within
-- the spelling's namespace). Empty when the spelling is free.
occupiedBy :: TargetScope -> RdrName -> [Name]
occupiedBy TargetScope{tsGlobalEnv} rdr =
  map gre_name (lookupGRERdr tsGlobalEnv rdr)

-- ---------------------------------------------------------------------
-- Import rendering

-- | The import declaration shapes import resolution generates.
data ImportForm
  = ImportModule
    -- ^ @import M@.
  | ImportMembers [(OccName, Maybe OccName)]
    -- ^ @import M (f, g, T (C))@.
  | ImportQualifiedAs ModuleName
    -- ^ @import qualified M as Q@ (or plain @import qualified M@ when
    -- the qualifier is the module's own name).

consolidate :: [(ModuleName, ImportItem)] -> [(ModuleName, ImportForm)]
consolidate raw = quals ++ members
  where
    items = nub raw
    quals = [ (m, ImportQualifiedAs q) | (m, ItemQualAs q) <- items ]
    members =
      [ (m, ImportMembers ms)
      | m <- nub [ m | (m, ItemMember{}) <- items ]
      , let ms = [ (occ, mp) | (m', ItemMember occ mp) <- items, m' == m ]
      ]

renderImports :: [(ModuleName, ImportForm)] -> AnnotatedImports
renderImports items = unsafeMkA (map renderImport items) 0

-- | Build an import declaration, annotated so it exact-prints with
-- idiomatic spacing. Symbolic names get parens
-- (@import Data.List ((\\\\))@).
renderImport :: (ModuleName, ImportForm) -> LImportDecl GhcPs
renderImport (m, form) = L noAnnSrcSpanDP0 $ case form of
  ImportModule -> base { ideclExt = ext Nothing Nothing }
  ImportQualifiedAs q
    | q == m ->
        base { ideclQualified = QualifiedPre, ideclExt = ext (Just qualTok) Nothing }
    | otherwise ->
        base { ideclQualified = QualifiedPre
             , ideclAs        = Just (L noAnnSrcSpanDP1 q)
             , ideclExt       = ext (Just qualTok) (Just (EpTok (dn 1))) }
  ImportMembers ms ->
    base { ideclImportList = Just (Exactly, importList (zipWith member [0 ..] ms))
         , ideclExt        = ext Nothing Nothing }
  where
    base = (simpleImportDecl m) { ideclName = L noAnnSrcSpanDP1 m }
    qualTok = EpTok (dn 1)
    ext qual as' = XImportDeclPass
      (EpAnn d0 (EpAnnImportDecl (EpTok d0) Nothing Nothing Nothing qual Nothing as') emptyComments)
      NoSourceText False
    member :: Int -> (OccName, Maybe OccName) -> LIE GhcPs
    member i (occ, Nothing) =
      setEntryDP (ieVar (mkIEName (mkRdrUnqual occ))) (SameLine (min i 1))
    member i (occ, Just p) =
      setEntryDP (mkTypeWithIE (mkRdrUnqual p) (mkRdrUnqual occ :| [])) (SameLine (min i 1))

-- | @(item, item)@ one space after the module name.
importList :: [LIE GhcPs] -> LocatedLI [LIE GhcPs]
importList items = L (EpAnn (dn 1) ann emptyComments) (withCommas items)
  where
    ann = noAnn { al_brackets = ListParens (EpTok d0) (EpTok d0) }
