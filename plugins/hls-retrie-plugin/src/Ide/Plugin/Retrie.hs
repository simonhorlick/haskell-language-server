{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeFamilies          #-}

{-# OPTIONS -Wno-orphans #-}

module Ide.Plugin.Retrie (descriptor, Log) where

import           Control.Exception.Safe               (Exception (..),
                                                       SomeException, assert,
                                                       catch, throwIO, try)
import           Control.Lens.Operators
import           Control.Monad                        (forM, unless, when)
import           Control.Monad.Error.Class            (MonadError (throwError))
import           Control.Monad.IO.Class               (MonadIO (liftIO))
import           Control.Monad.Trans.Class            (MonadTrans (lift))
import           Control.Monad.Trans.Except           (ExceptT (..), runExceptT)

import           Control.Monad.Trans.Maybe            (MaybeT)
import           Data.Aeson                           (FromJSON (..),
                                                       ToJSON (..))
import           Data.Bifunctor                       (second)
import           Data.Data
import qualified Data.HashSet                         as Set
import           Data.List.Extra                      (nubOrd, nubOrdOn, sortOn)
import qualified Data.Map                             as Map
import           Data.Monoid                          (First (First))
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as T
import           Development.IDE                      hiding (pluginHandlers)
import           Development.IDE.Core.Actions         (lookupMod)
import           Development.IDE.Core.PluginUtils
import           Development.IDE.Core.PositionMapping
import           Development.IDE.Core.Rules           (getSourceFileSource)
import           Development.IDE.Core.Shake           (ShakeExtras (ShakeExtras),
                                                       hiedbWriter, withHieDb)
import           Development.IDE.GHC.Compat           (GRHSs (GRHSs),
                                                       GenLocated (L), GhcPs,
                                                       GhcRn,
                                                       HsBindLR (FunBind),
                                                       HsExpr, HsGroup (..),
                                                       HsValBindsLR (..),
                                                       HscEnv, ImportDecl (..),
                                                       LHsExpr,
                                                       ModSummary (ModSummary, ms_hspp_buf, ms_mod),
                                                       ParsedModule, fun_id,
                                                       moduleNameString,
                                                       ms_hspp_opts,
                                                       nameModule_maybe,
                                                       nameOccName,
                                                       occNameString,
                                                       pattern RealSrcSpan,
                                                       pm_parsed_source,
                                                       srcSpanFile,
                                                       stringToUnit, topDir,
                                                       unLoc)
import qualified Development.IDE.GHC.Compat           as GHC
import           Development.IDE.GHC.Compat.Util      hiding (catch, try)
import           Development.IDE.GHC.ExactPrint       (GetAnnotatedParsedSource (GetAnnotatedParsedSource),
                                                       TransformT)
import           Development.IDE.Plugin.CodeAction    (newImportInsertRange)
import           Development.IDE.Spans.AtPoint        (LookupModule,
                                                       nameToLocation)
import           Development.IDE.Types.Shake          (WithHieDb)
import qualified GHC                                  as GHCGHC
import           GHC.Generics                         (Generic)
import qualified GHC.LanguageExtensions.Type          as LangExt (Extension (..))
import           GHC.Types.Name                       (isExternalName,
                                                       isVarName)
import           GHC.Types.Name.Occurrence            (mkVarOcc)
import           GHC.Types.Name.Set                   (elemNameSet, mkNameSet)
import           HieDb                                ((:.) (..))
import qualified HieDb
import           Ide.Plugin.Error                     (PluginError (PluginInternalError),
                                                       getNormalizedFilePathE)
import           Ide.Plugin.Resolve                   (mkCodeActionHandlerWithResolve)
import           Ide.PluginUtils
import           Ide.Types
import qualified Language.LSP.Protocol.Lens           as L
import           Language.LSP.Protocol.Message        as LSP
import           Language.LSP.Protocol.Types          as LSP
import           Language.LSP.Server                  (ProgressCancellable (Cancellable))
import           Retrie                               (Annotated (astA),
                                                       AnnotatedImports,
                                                       AnnotatedModule,
                                                       RenameInfo,
                                                       applyWithRenameInfo,
                                                       mkRenameInfo)
import           Retrie.CPP                           (CPP (NoCPP), parseCPP)
import           Retrie.ExactPrint                    (exactPrint, fix,
                                                       makeDeltaAst, transformA,
                                                       unsafeMkA)
import           Retrie.Expr                          (mkLocatedHsVar)
import           Retrie.Fixity                        (FixityEnv)
import           Retrie.Monad                         (runRetrie)
import           Retrie.Replace                       (Change (..),
                                                       Replacement (..))
import           Retrie.Rewrites.Function             (matchToRewrites)
import           System.FilePath                      (takeFileName)

import           Retrie.SYB                           (everything, listify, mkQ)
import           Retrie.Types
import           Retrie.Universe                      (Universe)


import           Data.Maybe                           (isNothing)
import           Ide.Plugin.Retrie.Dispatch           (dispatchRewrites,
                                                       triviallySelectable)
import           Ide.Plugin.Retrie.Extensions         (spliceExtensions,
                                                       spliceableInto)
import           Ide.Plugin.Retrie.Fixity
import           Ide.Plugin.Retrie.GHC                (greIsParentless,
                                                       lookupGREName)
import           Ide.Plugin.Retrie.Imports            (DefScope, mkDefScope,
                                                       mkTargetScope,
                                                       requalifyRewrite)
import           Ide.Plugin.Retrie.Transformer        (restrictToSite)

data Log
  = LogParsingModule FilePath
  | LogImportRefused String
  | forall a. Pretty a => LogResolve a

instance Pretty Log where
  pretty = \case
    LogParsingModule fp -> "Parsing module:" <+> pretty fp
    LogImportRefused reason -> "Inline refused by import resolution:" <+> pretty reason
    LogResolve l        -> pretty l

descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
  (defaultPluginDescriptor plId "Provides code actions to inline Haskell definitions")
    { pluginHandlers =
        mkCodeActionHandlerWithResolve
          (cmapWithPrio LogResolve recorder)
          provider
          (resolveProvider recorder)
    }

-- | Data stashed in a code action's @_data_@ field and passed back by the
-- client in the resolve request, identifying the rewrite to perform.
data RetrieResolveData
  = ResolveInlineAll RunRetrieInlineAllParams
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | We receive a resolve request when the user has selected a code action in
-- the UI. The client passes back the 'RetrieResolveData' we attached to the
-- code action, from which we compute the 'WorkspaceEdit' to attach.
resolveProvider :: Recorder (WithPriority Log) -> ResolveFunction IdeState RetrieResolveData Method_CodeActionResolve
resolveProvider recorder state _plId ca uri = \case
  ResolveInlineAll params -> resolveInlineAll recorder state ca uri params

-- | Everything needed to inline a definition into every file that
-- references it. The location identifies the definition the rewrite is
-- built from; the occurrence name and defining module identify the
-- function in the hiedb reference index.
data RunRetrieInlineAllParams = RunRetrieInlineAllParams
  { iaFromLocation :: !Location
  , iaToLocation   :: !(Maybe Location)
  , iaDefinition   :: !T.Text
  , iaOccName      :: !T.Text
  , iaModuleName   :: !(Maybe T.Text)
  , iaUnitId       :: !(Maybe T.Text)
  }
  deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- | The result of rewriting one target file with a prepared inline
-- rewrite. Failures are data rather than exceptions so a multi-target
-- caller can apply the edits that succeeded and report the files that
-- were left out.
data TargetOutcome
  = TargetEdited (Map.Map Uri [TextEdit])
    -- ^ The target's edits, each file's list ascending by position.
  | TargetSkipped
    -- ^ No requested call site in the file; nothing to edit.
  | TargetNotRewritable T.Text
    -- ^ The target cannot take the splice (it lacks an extension the
    -- body needs). The file must be left unchanged.
  | TargetFailed T.Text
    -- ^ The target could not be processed at all.

-- | Rewrite the call sites of one target file with an inline rewrite
-- built from the defining module. The target's own session, fixities
-- and rename info are looked up here rather than passed in: each
-- target may resolve names and operators differently.
rewriteTarget
  :: Recorder (WithPriority Log)
  -> IdeState
  -> [Rewrite Universe]
  -- ^ The inline rewrite, built from the defining module.
  -> [LangExt.Extension]
  -- ^ Extensions the rewrite's templates need enabled in the target.
  -> RenameInfo
  -- ^ Rename info of the defining module; combined with the target's
  -- own so spliced names render in a form valid at the target.
  -> DefScope
  -- ^ Scope view of the defining module, resolving the names the
  -- inlined body references and where they can be imported from.
  -> FixityEnv
  -- ^ Fixities of the operators the defining module uses. Fixities of
  -- imported operators (Prelude's included) are not in a module's own
  -- interface, so both sides' environments are looked up explicitly.
  -> Maybe GHCGHC.RealSrcSpan
  -- ^ 'Just': rewrite only the call site at this span ("inline this");
  -- 'Nothing': rewrite every call site in the file.
  -> NormalizedFilePath
  -> IO TargetOutcome
rewriteTarget recorder state inlineRewrite neededExts defRenameInfo defScope defFixities singleSite target = do
  inputs <- try @_ @SomeException $ do
    session <-
      hscEnv
        <$> useOrFail
              state
              "Retrie.GhcSessionDeps"
              (CallRetrieInternalError "no session deps")
              GhcSessionDeps
              target
    check <- useOrFail state "Retrie.TypeCheck" NoTypeCheck TypeCheck target
    targetFixities <-
      fixityEnvFor session (tmrTypechecked check) (tmrRenamed check)
    (cpp, annPs, contents) <-
      getCPPmodule recorder state session targetFixities $
        fromNormalizedFilePath target
    pure (session, check, targetFixities, cpp, annPs, contents)
  case inputs of
    Left err -> pure $ TargetFailed $ T.pack $ show err
    Right (session, check, targetFixities, cpp, annPs, contents)
      | Left reason <- spliceableInto neededExts (mkTargetScope check) ->
          pure $ TargetNotRewritable $ T.pack reason
      | otherwise -> do
      let renameInfo = defRenameInfo <> mkRenameInfo (tmrRenamed check)
          -- rewrite the templates using the import spellings in the target
          requalified =
            map
              (maybe id restrictToSite singleSite
                . requalifyRewrite
                    (logWith recorder Debug . LogImportRefused)
                    (topDir (GHC.hsc_dflags session))
                    defScope
                    (mkTargetScope check))
              inlineRewrite
      result <-
        try @_ @SomeException $
          runRetrie
            (defFixities <> targetFixities)
            (applyWithRenameInfo renameInfo requalified)
            cpp
      pure $ case result of
        Left err ->
          TargetFailed $ "Retrie - crashed with: " <> T.pack (show err)
        Right (_, _, NoChange) -> TargetSkipped
        Right (_, _, Change replacements imports) ->
          case replacements of
            [] -> TargetSkipped
            selected ->
              TargetEdited $ asEditMap $
                asTextEdits target annPs contents (Change selected imports)

-- | Inline a definition into every file that references it. Rewrites
-- each target file independently and reports any that fail.
resolveInlineAll
  :: Recorder (WithPriority Log)
  -> IdeState
  -> CodeAction
  -> Uri
  -> RunRetrieInlineAllParams
  -> ExceptT PluginError (HandlerM Config) CodeAction
resolveInlineAll recorder state ca uri RunRetrieInlineAllParams{..} = ExceptT $
  pluginWithIndefiniteProgress (ca ^. L.title) Nothing Cancellable $ \ msg -> runExceptT $ do
    tgtPath <- getNormalizedFilePathE uri
    srcPath <- getNormalizedFilePathE $ getLocationUri iaFromLocation

    let fromSp = rangeToRealSrcSpan srcPath $ getLocationRange iaFromLocation
        intoSp = rangeToRealSrcSpan tgtPath <$> getLocationRange <$> iaToLocation

    parsed <- runActionE "retrie" state $ useE GetAnnotatedParsedSource srcPath

    (session, _) <- runActionE "retrie" state $ useWithStaleE GhcSessionDeps srcPath
    (check, _) <- runActionE "retrie" state $ useWithStaleE TypeCheck srcPath

    defMod <- liftIO $ fixedModule (hscEnv session) check parsed

    rewrites <- liftIO $ constructInlineFromIdentifer (fmSource defMod) fromSp

    when (null rewrites) $
      throwError $
        PluginInternalError
          "no inline rewrite could be built; the document may have changed"

    refFiles <- case (iaModuleName, iaUnitId) of
      (Just modName, Just unit)
        | isNothing iaToLocation ->
        liftIO $ referencingFiles state iaOccName modName unit
      -- a name without a module is locally bound; nothing outside the
      -- requesting file can reference it
      _ -> pure []

    let targets = nubOrd (tgtPath : srcPath : refFiles)
        neededExts =
          spliceExtensions [ astA (tTemplate t) | Query{qResult = (t, _)} <- rewrites ]
        defRenameInfo = mkRenameInfo (tmrRenamed check)
        defScope = mkDefScope defRenameInfo check

    outcomes <- forM targets $ \ target -> do
      lift $ msg $ T.pack $ takeFileName (fromNormalizedFilePath target)
      liftIO $
        rewriteTarget
          recorder
          state
          rewrites
          neededExts
          defRenameInfo
          defScope
          (fmFixities defMod)
          intoSp
          target

    let edits = Map.unionsWith (<>) [m | TargetEdited m <- outcomes]
        reported =
          [ (target, reason)
          | (target, outcome) <- zip targets outcomes
          , reason <- case outcome of
              TargetNotRewritable reason -> [reason]
              TargetFailed reason        -> [reason]
              -- the reference index says this file uses the function,
              -- yet nothing was rewritten: its sites were refused. The
              -- defining file is exempt -- the definition itself is
              -- reference enough, with no call site behind it
              TargetSkipped
                | target /= srcPath
                , target `elem` refFiles ->
                    [ "no call site could be rewritten; bindings there"
                        <> " may capture variables of the inlined body,"
                        <> " or the body may reference names that cannot"
                        <> " be imported there"
                    ]
              _ -> []
          ]

    -- per-file failures do not abort the edit: apply what succeeded and
    -- tell the user what was left out
    unless (null reported) $
      lift $
        pluginSendNotification SMethod_WindowShowMessage $
          ShowMessageParams MessageType_Warning $ T.unlines $
            (ca ^. L.title <> ": some files were not rewritten:")
              : [ "- " <> T.pack (fromNormalizedFilePath t) <> ": " <> reason
                | (t, reason) <- reported
                ]
    return $ ca & L.edit ?~ WorkspaceEdit (Just edits) Nothing Nothing

-- | Files the hiedb reference index knows use the given name, by
-- occurrence name and defining module. The index only covers modules
-- that have been typechecked and indexed, so it can lag the state of
-- the project; the caller always adds the requesting and defining
-- files itself. Only the variable namespace is searched: inline
-- candidates are identifiers found in expression position.
referencingFiles :: IdeState -> T.Text -> T.Text -> T.Text -> IO [NormalizedFilePath]
referencingFiles state occ modName unit = do
  let extras = shakeExtras state
  rows <- withHieDb extras $ \hieDb ->
    HieDb.findReferences
      hieDb
      True
      (mkVarOcc (T.unpack occ))
      (Just (GHC.mkModuleName (T.unpack modName)))
      (Just (stringToUnit (T.unpack unit)))
      []
  pure
    [ toNormalizedFilePath' file
    | (row :. info) <- rows
    -- compiler-generated occurrences (deriving, selectors) are not call
    -- sites; see Note [Generated references] in Ide.Plugin.Rename
    , not (HieDb.refIsGenerated row)
    , Just file <- [HieDb.modInfoSrcFile info]
    ]

-------------------------------------------------------------------------------

provider :: PluginMethodHandler IdeState Method_TextDocumentCodeAction
provider state _plId (CodeActionParams _ _ (TextDocumentIdentifier uri) range ca) = do
  let (LSP.CodeActionContext _diags _monly _) = ca
  nfp <- getNormalizedFilePathE uri

  (ModSummary{ms_mod}, topLevelBinds, posMapping, rdrEnv) <-
    runActionE "retrie" state $
      getBinds nfp

  let extras@ShakeExtras{withHieDb, hiedbWriter} = shakeExtras state

  range <- fromCurrentRangeE posMapping range
  inlineSuggestions <-
    liftIO $
      runIdeAction "" extras $
        suggestBindInlines rdrEnv ms_mod topLevelBinds range withHieDb (lookupMod hiedbWriter)
  let inlineActions =
        [ mkCodeAction title CodeActionKind_RefactorInline resolveData
        | (title, resolveData) <- inlineSuggestions
        ]
  return $ InL [InR c | c <- inlineActions]

-- | A code action carrying only its resolve data; the edit is computed in
-- 'resolveProvider' once the action is selected.
mkCodeAction :: T.Text -> CodeActionKind -> RetrieResolveData -> CodeAction
mkCodeAction title kind resolveData =
  CodeAction title (Just kind) Nothing Nothing Nothing Nothing Nothing (Just (toJSON resolveData))

getLocationUri :: Location -> Uri
getLocationUri Location{_uri} = _uri

getLocationRange :: Location -> Range
getLocationRange Location{_range} = _range

getBinds
  :: NormalizedFilePath
  -> ExceptT
       PluginError
       Action
       ( ModSummary
       , [HsBindLR GhcRn GhcRn]
       , PositionMapping
       , GHC.GlobalRdrEnv
       )
getBinds nfp = do
  (tm, posMapping) <- useWithStaleE TypeCheck nfp
  let rn = tmrRenamed tm
  case rn of
#if MIN_VERSION_ghc(9,9,0)
    (HsGroup{hs_valds}, _, _, _, _) -> do
#else
    (HsGroup{hs_valds}, _, _, _) -> do
#endif
      topLevelBinds <- case hs_valds of
        ValBinds{} -> throwError $ PluginInternalError "getBinds: ValBinds not supported"
        XValBindsLR (GHC.NValBinds binds _sigs :: GHC.NHsValBindsLR GhcRn) ->
          pure
            [ decl
#if MIN_VERSION_ghc(9,11,0)
            | (_, listBinds) <- binds
            , L _ decl <- listBinds
#else
            | (_, bagBinds) <- binds
            , L _ decl <- bagToList bagBinds
#endif
            ]
      return (tmrModSummary tm, topLevelBinds, posMapping, GHC.tcg_rdr_env (tmrTypechecked tm))

-- | Inline suggestions for the request range: identifiers used in a
-- RHS for which we have a source definition, and the names bindings
-- define (top-level or bound in a where clause or let block), which
-- offer inlining the definition into its call sites. Identifiers not
-- bound by a function equation (parameters, pattern binders, record
-- selectors, class methods) are not offered; see 'inlinableName'.
suggestBindInlines
  :: GHC.GlobalRdrEnv
  -> GHC.Module
  -> [HsBindLR GhcRn GhcRn]
  -> Range
  -> WithHieDb
  -> (FilePath -> GHCGHC.ModuleName -> GHCGHC.Unit -> Bool -> MaybeT IdeAction Uri)
  -> IdeAction [(T.Text, RetrieResolveData)]
suggestBindInlines rdrEnv thisMod binds range hie lookupMod = do
  identifiers <- definedIdentifiers (inlinableName rdrEnv thisMod binds)
  return $
    concatMap suggestions (Set.toList identifiers)
      <> concatMap binderSuggestions (Set.toList binderIdentifiers)
  where
    suggestions (name, mbModUnit, siteLoc, srcLoc) =
      let
        printedName = printOutputable name
        single =
          RunRetrieInlineAllParams
            { iaFromLocation = srcLoc
            , iaToLocation = Just siteLoc
            , iaDefinition = printedName
            , iaOccName = T.pack (occNameString name)
            , iaModuleName = fst <$> mbModUnit
            , iaUnitId = snd <$> mbModUnit
            }
        everywhere =
          RunRetrieInlineAllParams
            { iaFromLocation = srcLoc
            , iaToLocation = Nothing -- no site restriction
            , iaDefinition = printedName
            , iaOccName = T.pack (occNameString name)
            , iaModuleName = fst <$> mbModUnit
            , iaUnitId = snd <$> mbModUnit
            }
       in
        [ ("Inline " <> printedName, ResolveInlineAll single)
        , ("Inline " <> printedName <> " everywhere", ResolveInlineAll everywhere)
        ]

    -- a definition has no site of its own to inline into, so only the
    -- everywhere action is offered on a binder
    binderSuggestions (name, mbModUnit, defLoc) =
      let printedName = printOutputable name
       in [ ( "Inline " <> printedName <> " everywhere"
            , ResolveInlineAll
                RunRetrieInlineAllParams
                  { iaFromLocation = defLoc
                  , iaToLocation = Nothing
                  , iaDefinition = printedName
                  , iaOccName = T.pack (occNameString name)
                  , iaModuleName = fst <$> mbModUnit
                  , iaUnitId = snd <$> mbModUnit
                  }
            )
          ]

    -- names of the bindings the request range is on; unlike the use
    -- sites below these need no hiedb lookup -- the binder is its own
    -- definition
    binderIdentifiers = everything (<>) (mempty `mkQ` getBinderDetails) binds

    getBinderDetails
      :: HsBindLR GhcRn GhcRn
      -> Set.HashSet (GHC.OccName, Maybe (T.Text, T.Text), Location)
    getBinderDetails FunBind{fun_id = lname}
      | name <- unLoc lname
      , Just defLoc <- srcSpanToLocation (GHC.getLocA lname)
      , range `isSubrangeOf` getLocationRange defLoc =
          Set.singleton (nameOccName name, nameModUnit name, defLoc)
    getBinderDetails _ = mempty

    definedIdentifiers funBindLocally =
      -- we search for candidates to inline in RHSs only, skipping LHSs
      everything (<>) (pure mempty `mkQ` getGRHSIdentifierDetails funBindLocally hie lookupMod) binds

    getGRHSIdentifierDetails
      :: (GHC.Name -> Bool)
      -> WithHieDb
      -> (FilePath -> GHCGHC.ModuleName -> GHCGHC.Unit -> Bool -> MaybeT IdeAction Uri)
      -> GRHSs GhcRn (LHsExpr GhcRn)
      -> IdeAction (Set.HashSet (GHC.OccName, Maybe (T.Text, T.Text), Location, Location))
    getGRHSIdentifierDetails funBindLocally a b it@GRHSs{} =
      -- we only select candidates for which we have source code
      everything (<>) (pure mempty `mkQ` getDefinedIdentifierDetailsViaHieDb funBindLocally a b) it

#if MIN_VERSION_ghc(9,13,0)
    getDefinedIdentifierDetailsViaHieDb
      :: (GHC.Name -> Bool)
      -> WithHieDb
      -> LookupModule IdeAction
      -> GHCGHC.LIdOccP GhcRn
      -> IdeAction (Set.HashSet (GHC.OccName, Maybe (T.Text, T.Text), Location, Location))
#else
    getDefinedIdentifierDetailsViaHieDb
      :: (GHC.Name -> Bool)
      -> WithHieDb
      -> LookupModule IdeAction
      -> GHC.LIdP GhcRn
      -> IdeAction (Set.HashSet (GHC.OccName, Maybe (T.Text, T.Text), Location, Location))
#endif
    getDefinedIdentifierDetailsViaHieDb funBindLocally withHieDb lookupModule lname | name <- GHCGHC.getName (unLoc lname) =
      case srcSpanToLocation (GHC.getLocA lname) of
        Just siteLoc
          | siteRange <- getLocationRange siteLoc
          , range `isSubrangeOf` siteRange
          , isVarName name
          , funBindLocally name -> do
              mbSrcLocation <- nameToLocation withHieDb lookupModule name
              return $
                maybe
                  mempty
                  (Set.fromList . map (nameOccName name,nameModUnit name,siteLoc,) . filter (/= siteLoc))
                  mbSrcLocation
        _ -> pure mempty

-- | Whether inlining may be offered for a name: it is bound by a
-- function equation in this module ('FunBind', top-level or nested),
-- or imported as a plain top-level binding. Parameters, pattern
-- binders, record selectors, class methods and constructors have no
-- equation the rewrite could splice, so an offer could only fail at
-- resolve.
inlinableName :: GHC.GlobalRdrEnv -> GHC.Module -> [HsBindLR GhcRn GhcRn] -> GHC.Name -> Bool
inlinableName rdrEnv thisMod binds = \name ->
  name `elemNameSet` localFunBinds
    || (isExternalName name && GHC.nameModule name /= thisMod && importedFunction name)
  where
    localFunBinds = mkNameSet [unLoc fun_id | FunBind{fun_id} <- listify isFunBind binds]
    isFunBind :: HsBindLR GhcRn GhcRn -> Bool
    isFunBind FunBind{} = True
    isFunBind _         = False
    importedFunction = maybe False greIsParentless . lookupGREName rdrEnv

-- | The module and unit an identifier comes from, when it has one:
-- locally bound names have none.
nameModUnit :: GHC.Name -> Maybe (T.Text, T.Text)
nameModUnit name =
  (\m ->
    ( T.pack (moduleNameString (GHC.moduleName m))
    , T.pack (GHC.unitString (GHC.moduleUnit m))
    ))
    <$> nameModule_maybe name

-------------------------------------------------------------------------------
-- Retrie driving code

data CallRetrieError
  = CallRetrieInternalError String NormalizedFilePath
  | NoParse NormalizedFilePath
  | GHCParseError NormalizedFilePath String
  | NoTypeCheck NormalizedFilePath
  deriving (Eq)

instance Show CallRetrieError where
  show (CallRetrieInternalError msg f) = msg <> " - " <> fromNormalizedFilePath f
  show (NoParse f) = "Cannot parse: " <> fromNormalizedFilePath f
  show (GHCParseError f m) = "Cannot parse " <> fromNormalizedFilePath f <> " : " <> m
  show (NoTypeCheck f) = "File does not typecheck: " <> fromNormalizedFilePath f

instance Exception CallRetrieError

useOrFail
  :: IdeRule r v
  => IdeState
  -> String
  -> (NormalizedFilePath -> CallRetrieError)
  -> r
  -> NormalizedFilePath
  -> IO (RuleResult r)
useOrFail state lbl mkException rule f =
  useRule lbl state rule f >>= maybe (liftIO $ throwIO $ mkException f) return

fixAnns :: ParsedModule -> Annotated GHC.ParsedSource
fixAnns GHC.ParsedModule{pm_parsed_source} = unsafeMkA (makeDeltaAst pm_parsed_source) 0

-- | Generate retrie rewrites for each clause.
constructfromFunMatches
  :: Annotated [GHCGHC.LocatedA (ImportDecl GhcPs)]
  -> GHCGHC.LocatedN GHCGHC.RdrName
  -> GHCGHC.MatchGroup GhcPs (GHCGHC.LocatedA (HsExpr GhcPs))
  -> TransformT IO [Rewrite Universe]
constructfromFunMatches imps fun_id fun_matches = do
  fe <- mkLocatedHsVar fun_id
  rewrites <-
    concat
      <$> forM (unLoc $ GHC.mg_alts fun_matches) (matchToRewrites fe imps LeftToRight)
  let urewrites = toURewrite <$> rewrites
  -- traceShowM $ map showQuery urewrites
  assert (not $ null urewrites) $
    return urewrites

-- showQuery :: Rewrite Universe -> String
-- showQuery = ppRewrite
--
-- showQuery :: Rewrite (LHsExpr GhcPs) -> String
-- showQuery q = unlines
--     [ "template: " <> show (hash (printOutputable . showAstData NoBlankSrcSpan . astA . tTemplate . fst . qResult $ q))
--     , "quantifiers: " <> show (hash (T.pack (show(Ext.toList $ qQuantifiers q))))
--     , "matcher: " <> show (hash (printOutputable . showAstData NoBlankSrcSpan . astA . qPattern $ q))
--     ]
--
-- s :: Data a => a -> String
-- s = T.unpack . printOutputable . showAstData NoBlankSrcSpan
--         NoBlankEpAnnotations

constructInlineFromIdentifer :: Data a => Annotated (GenLocated l a) -> GHCGHC.RealSrcSpan -> IO [Rewrite Universe]
constructInlineFromIdentifer originParsedModule originSpan = do
  -- traceM $ s $ astA originParsedModule
  fmap astA $ transformA originParsedModule $ \(L _ m) -> do
    let ast = everything (<>) (First Nothing `mkQ` matcher) m
        matcher
          :: HsBindLR GhcPs GhcPs
          -> First
               ( GHCGHC.LocatedN GHCGHC.RdrName
               , GHCGHC.MatchGroup GhcPs (GHCGHC.LocatedA (HsExpr GhcPs))
               )
        matcher FunBind{fun_id, fun_matches}
          -- trace (show (GHC.getLocA fun_id) <> ": " <> s fun_id) False = undefined
          | RealSrcSpan sp _ <- GHC.getLocA fun_id
          , sp == originSpan =
            First $ Just (fun_id, fun_matches)
        matcher _ = First Nothing
    case ast of
      First (Just (fun_id, fun_matches))
        | not (triviallySelectable fun_matches) ->
          -- if we can't statically determine the clause, generate an
          -- expression that keeps the clause dispatch
          map toURewrite <$> dispatchRewrites fun_id fun_matches
        | otherwise -> do
          constructfromFunMatches mempty fun_id fun_matches
      -- no definition at the recorded span: the document changed since
      -- the action was offered. An empty rewrite list is an error the
      -- callers report
      _ -> return []

-- | Group 'TextEdit's by file.
asEditMap :: [(Uri, TextEdit)] -> Map.Map Uri [TextEdit]
asEditMap =
  Map.map (sortOn (^. L.range . L.start))
    . Map.fromListWith (++)
    . map (second pure)

asTextEdits :: NormalizedFilePath -> GHCGHC.ParsedSource -> T.Text -> Change -> [(Uri, TextEdit)]
asTextEdits _ _ _ NoChange = []
asTextEdits target ps contents (Change reps imports) =
  case replacementTextEdits reps of
    -- 'addImports' requests imports even in files where no rewrite
    -- fired; an untouched file must not gain them
    []    -> []
    edits -> edits <> importTextEdits target ps contents imports

replacementTextEdits :: [Replacement] -> [(Uri, TextEdit)]
replacementTextEdits reps =
  [ (filePathToUri spanLoc, edit)
  | Replacement{..} <- nubOrdOn (realSpan . replLocation) reps
  , (RealSrcSpan rspan _) <- [replLocation]
  , let spanLoc = unpackFS $ srcSpanFile rspan
  , let edit = TextEdit (realSrcSpanToRange rspan) (T.pack replReplacement)
  ]

-- | The imports a rewrite requested via 'addImports', as one insertion
-- edit below the target's last import -- the same spot the refactor
-- plugin's import actions use. Imports the target already has, and
-- imports of the target itself, are dropped. Rendering through ppr
-- normalises annotations away, so the rendered text doubles as the
-- dedupe key.
importTextEdits
  :: NormalizedFilePath -> GHCGHC.ParsedSource -> T.Text -> [AnnotatedImports] -> [(Uri, TextEdit)]
importTextEdits target ps contents annIs =
  [ (filePathToUri (fromNormalizedFilePath target), TextEdit range text)
  | not (null newImports)
  , Just (range, indent) <- [newImportInsertRange ps contents]
  , let sep = "\n" <> T.replicate indent " "
        text = T.intercalate sep (map snd newImports) <> sep
  ]
  where
    L _ hsmod = ps
    render = printOutputable . unLoc
    existing = Set.fromList (map render (GHCGHC.hsmodImports hsmod))
    selfName = unLoc <$> GHCGHC.hsmodName hsmod
    newImports =
      nubOrdOn fst
        [ (render i, importText i)
        | i <- concatMap astA annIs
        , Just (unLoc (ideclName (unLoc i))) /= selfName
        , not (render i `Set.member` existing)
        ]
    -- A declaration that came in with a real span was parsed, so it
    -- exact-prints with its idiomatic spacing ("import M (f)"); a
    -- generated span means the declaration was built programmatically
    -- without annotations ('toImportDecl') and only ppr can render it.
    importText i = case GHC.getLocA i of
      RealSrcSpan _ _ -> T.strip (T.pack (exactPrint i))
      _               -> render i

-------------------------------------------------------------------------------
-- Rule wrappers

_useRuleBlocking
  , _useRuleStale
  , useRule
    :: IdeRule k v
    => String
    -> IdeState
    -> k
    -> NormalizedFilePath
    -> IO (Maybe (RuleResult k))
_useRuleBlocking label state rule f = runAction label state (use rule f)
_useRuleStale label state rule f =
  fmap fst
    <$> runIdeAction label (shakeExtras state) (useWithStaleFast rule f)

-- | Chosen approach for calling ghcide Shake rules
useRule label = _useRuleStale ("Retrie." <> label)

-- | The retrie view of a target file, together with the inputs
-- 'importTextEdits' needs to place new imports: the real-span parse
-- and the current file contents.
getCPPmodule :: Recorder (WithPriority Log) -> IdeState -> HscEnv -> FixityEnv -> FilePath -> IO (CPP AnnotatedModule, GHCGHC.ParsedSource, T.Text)
getCPPmodule recorder state session fixities t = do
  -- TODO: is it safe to drop this makeAbsolute?
  let nt = toNormalizedFilePath' $ (toAbsolute $ rootDir state) t
  let getParsedModule contents = do
        modSummary <-
          msrModSummary
            <$> useOrFail state "Retrie.GetModSummary" (CallRetrieInternalError "file not found") GetModSummary nt
        let ms' =
              modSummary
                { ms_hspp_buf =
                    Just (stringToStringBuffer contents)
                }
        logWith recorder Info $ LogParsingModule t
        parsed <-
          evalGhcEnv session (GHCGHC.parseModule ms')
            `catch` \e -> throwIO (GHCParseError nt (show @SomeException e))
        transformA (fixAnns parsed) (fix fixities)

  contents <-
    T.decodeUtf8 <$> runAction "Retrie.GetFileContents" state (getSourceFileSource nt)

  pm <- useOrFail state "Retrie.GetParsedModule" NoParse GetParsedModule nt

  let usesCpp = GHC.xopt LangExt.Cpp (ms_hspp_opts (GHC.pm_mod_summary pm))
  cpp <-
    if usesCpp
      then parseCPP getParsedModule contents
      else NoCPP <$> transformA (fixAnns pm) (fix fixities)
  pure (cpp, GHC.pm_parsed_source pm, contents)
