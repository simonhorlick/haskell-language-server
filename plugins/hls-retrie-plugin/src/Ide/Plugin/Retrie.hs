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
import qualified Data.ByteString                      as BS
import           Data.Data
import qualified Data.HashSet                         as Set
import           Data.List.Extra                      (nubOrd, nubOrdOn, sortOn)
import qualified Data.Map                             as Map
import           Data.Monoid                          (First (First))
import qualified Data.Text                            as T
import qualified Data.Text.Encoding                   as T
import qualified Data.Text.Utf16.Rope.Mixed           as Rope
import           Development.IDE                      hiding (pluginHandlers)
import           Development.IDE.Core.Actions         (lookupMod)
import           Development.IDE.Core.PluginUtils
import           Development.IDE.Core.PositionMapping
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
                                                       ModSummary (ms_hspp_buf),
                                                       ParsedModule, fun_id,
                                                       moduleNameString,
                                                       nameModule_maybe,
                                                       nameOccName,
                                                       occNameString,
                                                       pattern RealSrcSpan,
                                                       pm_parsed_source,
                                                       srcSpanFile,
                                                       stringToUnit, unLoc)
import qualified Development.IDE.GHC.Compat           as GHC
import           Development.IDE.GHC.Compat.Util      hiding (catch, try)
import           Development.IDE.GHC.ExactPrint       (GetAnnotatedParsedSource (GetAnnotatedParsedSource),
                                                       TransformT)
import           Development.IDE.Spans.AtPoint        (LookupModule,
                                                       nameToLocation)
import           Development.IDE.Types.Shake          (WithHieDb)
import qualified GHC                                  as GHCGHC
import           GHC.Generics                         (Generic)
import           GHC.Iface.Ext.Types                  (BindType (..),
                                                       ContextInfo (..),
                                                       identInfo)
import           GHC.Types.Name                       (isVarName)
import           GHC.Types.Name.Occurrence            (mkVarOcc)
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
                                                       AnnotatedModule,
                                                       RenameInfo,
                                                       applyWithRenameInfo,
                                                       mkRenameInfo)
import           Retrie.CPP                           (CPP (NoCPP), parseCPP)
import           Retrie.ExactPrint                    (fix, makeDeltaAst,
                                                       transformA, unsafeMkA)
import           Retrie.Expr                          (mkLocatedHsVar)
import           Retrie.Fixity                        (FixityEnv)
import           Retrie.Monad                         (runRetrie)
import           Retrie.Replace                       (Change (..),
                                                       Replacement (..))
import           Retrie.Rewrites.Function             (matchToRewrites)
import           System.FilePath                      (takeFileName)

import           Retrie.SYB                           (everything, mkQ)
import           Retrie.Types
import           Retrie.Universe                      (Universe)


import           Data.Maybe                           (isNothing)
import           Ide.Plugin.Retrie.Dispatch           (dispatchRewrites,
                                                       triviallySelectable)
import           Ide.Plugin.Retrie.Fixity

data Log
  = LogParsingModule FilePath
  | forall a. Pretty a => LogResolve a

instance Pretty Log where
  pretty = \case
    LogParsingModule fp -> "Parsing module:" <+> pretty fp
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
  { inlineAllFromThisLocation :: !Location
  , inlineAllIntoThisLocation :: !(Maybe Location)
  , inlineAllDefinition       :: !T.Text
  , inlineAllOccName          :: !T.Text
  , inlineAllModuleName       :: !(Maybe T.Text)
  , inlineAllUnitId           :: !(Maybe T.Text)
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
    -- ^ A requested site was matched but produced no edit -- retrie
    -- refuses a site when a binding there would capture a variable of
    -- the inlined body. The file must be left unchanged.
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
  -> RenameInfo
  -- ^ Rename info of the defining module; combined with the target's
  -- own so spliced names render in a form valid at the target.
  -> FixityEnv
  -- ^ Fixities of the operators the defining module uses. Fixities of
  -- imported operators (Prelude's included) are not in a module's own
  -- interface, so both sides' environments are looked up explicitly.
  -> Maybe GHCGHC.RealSrcSpan
  -- ^ 'Just': rewrite only the call site at this span ("inline this");
  -- 'Nothing': rewrite every call site in the file.
  -> NormalizedFilePath
  -> IO TargetOutcome
rewriteTarget recorder state inlineRewrite defRenameInfo defFixities singleSite target = do
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
    cpp <-
      getCPPmodule recorder state session targetFixities $
        fromNormalizedFilePath target
    pure (check, targetFixities, cpp)
  case inputs of
    Left err -> pure $ TargetFailed $ T.pack $ show err
    Right (check, targetFixities, cpp) -> do
      let renameInfo = defRenameInfo <> mkRenameInfo (tmrRenamed check)
      result <-
        try @_ @SomeException $
          runRetrie
            (defFixities <> targetFixities)
            (applyWithRenameInfo renameInfo inlineRewrite)
            cpp
      pure $ case result of
        Left err ->
          TargetFailed $ "Retrie - crashed with: " <> T.pack (show err)
        Right (_, _, NoChange) -> TargetSkipped
        Right (_, _, Change replacements imports) ->
          -- When a single site was requested, 'requalifyRewrite'
          -- already refused every match not containing it, so each
          -- replacement here is that site's. No imports either when
          -- nothing was spliced in.
          case replacements of
            [] -> TargetSkipped
            selected ->
              TargetEdited $ asEditMap $ asTextEdits (Change selected imports)

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
    nfp <- getNormalizedFilePathE uri
    nfpSource <- getNormalizedFilePathE $ getLocationUri inlineAllFromThisLocation
    astSrc <- runActionE "retrie" state $ useE GetAnnotatedParsedSource nfpSource
    let fromRange = rangeToRealSrcSpan nfpSource $ getLocationRange inlineAllFromThisLocation
        intoRange = rangeToRealSrcSpan nfp <$> getLocationRange <$> inlineAllIntoThisLocation
    (sessionSource, _) <- runActionE "retrie" state $ useWithStaleE GhcSessionDeps nfpSource
    (checkSource, _) <- runActionE "retrie" state $ useWithStaleE TypeCheck nfpSource

    defMod <- liftIO $ fixedModule (hscEnv sessionSource) checkSource astSrc

    let defRenameInfo = mkRenameInfo (tmrRenamed checkSource)
        defScope = mkDefScope defRenameInfo checkSource

    inlineRewrite <- liftIO $ constructInlineFromIdentifer (fmSource defMod) fromRange

    when (null inlineRewrite) $
      throwError $
        PluginInternalError
          "no inline rewrite could be built; the document may have changed"

    refFiles <- case (inlineAllModuleName, inlineAllUnitId) of
      (Just modName, Just unit)
        | isNothing inlineAllIntoThisLocation ->
        liftIO $ referencingFiles state inlineAllOccName modName unit
      -- a name without a module is locally bound; nothing outside the
      -- requesting file can reference it
      _ -> pure []

    let targets = nubOrd (nfp : nfpSource : refFiles)

    outcomes <- forM targets $ \ target -> do
      lift $ msg $ T.pack $ takeFileName (fromNormalizedFilePath target)
      liftIO $
        rewriteTarget
          recorder
          state
          inlineRewrite
          defRenameInfo
          (fmFixities defMod)
          intoRange
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
                | target /= nfpSource
                , target `elem` refFiles ->
                    [ "no call site could be rewritten; bindings there"
                        <> " may capture variables of the inlined body"
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

  (topLevelBinds, posMapping) <-
    runActionE "retrie" state $
      getBinds nfp

  let extras@ShakeExtras{withHieDb, hiedbWriter} = shakeExtras state

  range <- fromCurrentRangeE posMapping range
  inlineSuggestions <-
    liftIO $
      runIdeAction "" extras $
        suggestBindInlines nfp topLevelBinds range withHieDb (lookupMod hiedbWriter)
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
       ( [HsBindLR GhcRn GhcRn]
       , PositionMapping
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
      return (topLevelBinds, posMapping)

-- | Inline suggestions for the request range: identifiers used in a
-- RHS for which we have a source definition, and the names bindings
-- define (top-level or bound in a where clause or let block), which
-- offer inlining the definition into its call sites. Identifiers the
-- module's own HIE occurrences show to be bound by something other
-- than a function equation (parameters, pattern binders, record
-- selectors) are not offered; see 'hasFunBindOccurrence'.
suggestBindInlines
  :: NormalizedFilePath
  -> [HsBindLR GhcRn GhcRn]
  -> Range
  -> WithHieDb
  -> (FilePath -> GHCGHC.ModuleName -> GHCGHC.Unit -> Bool -> MaybeT IdeAction Uri)
  -> IdeAction [(T.Text, RetrieResolveData)]
suggestBindInlines nfp binds range hie lookupMod = do
  mbHar <- useWithStaleFast GetHieAst nfp
  let funBindLocally = maybe (const True) (hasFunBindOccurrence . fst) mbHar
  identifiers <- definedIdentifiers funBindLocally
  return $
    concatMap suggestions (Set.toList identifiers)
      <> concatMap binderSuggestions (Set.toList binderIdentifiers)
  where
    suggestions (name, mbModUnit, siteLoc, srcLoc) =
      let
        printedName = printOutputable name
        single =
          RunRetrieInlineAllParams
            { inlineAllFromThisLocation = srcLoc
            , inlineAllIntoThisLocation = Just siteLoc
            , inlineAllDefinition = printedName
            , inlineAllOccName = T.pack (occNameString name)
            , inlineAllModuleName = fst <$> mbModUnit
            , inlineAllUnitId = snd <$> mbModUnit
            }
        everywhere =
          RunRetrieInlineAllParams
            { inlineAllFromThisLocation = srcLoc
            , inlineAllIntoThisLocation = Nothing -- no site restriction
            , inlineAllDefinition = printedName
            , inlineAllOccName = T.pack (occNameString name)
            , inlineAllModuleName = fst <$> mbModUnit
            , inlineAllUnitId = snd <$> mbModUnit
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
                  { inlineAllFromThisLocation = defLoc
                  , inlineAllIntoThisLocation = Nothing
                  , inlineAllDefinition = printedName
                  , inlineAllOccName = T.pack (occNameString name)
                  , inlineAllModuleName = fst <$> mbModUnit
                  , inlineAllUnitId = snd <$> mbModUnit
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

-- | Whether inlining may be offered for a name, judged from the
-- module's own HIE occurrences. A name bound in this module is
-- inlinable only when it is bound by a function equation ('FunBind'):
-- its binding occurrence carries @ValBind RegularBind@. Parameters
-- and lambda\/case\/do\/pattern binders carry only 'PatternBind',
-- record selectors carry 'RecField' contexts alongside the 'ValBind'
-- of their generated selector, and class\/instance methods never
-- carry @RegularBind@ -- none of these have an equation the rewrite
-- could splice, so an offer could only fail at resolve. A name with
-- no binding occurrence here is imported and is accepted: only
-- top-level bindings are visible across modules, so the parameter
-- case cannot arise, and the definition's shape is checked when the
-- rewrite is built from the defining module.
hasFunBindOccurrence :: HieAstResult -> GHC.Name -> Bool
hasFunBindOccurrence HAR{refMap} name =
  case Map.lookup (Right name) refMap of
    Nothing -> True
    Just occurrences ->
      let bindingOccs =
            [ info
            | (_, details) <- occurrences
            , let info = identInfo details
            , any isBindingCtx info
            ]
       in null bindingOccs || any isFunBindOcc bindingOccs
  where
    isBindingCtx = \case
      ValBind{}     -> True
      PatternBind{} -> True
      TyVarBind{}   -> True
      ClassTyDecl{} -> True
      Decl{}        -> True
      MatchBind     -> True
      _             -> False
    isFunBindOcc info =
      any isRegularValBind info && not (any isRecField info)
    isRegularValBind = \case
      ValBind RegularBind _ _ -> True
      _                       -> False
    isRecField = \case
      RecField{} -> True
      _          -> False

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

asTextEdits :: Change -> [(Uri, TextEdit)]
asTextEdits NoChange = []
asTextEdits (Change reps _imports) =
  [ (filePathToUri spanLoc, edit)
  | Replacement{..} <- nubOrdOn (realSpan . replLocation) reps
  , (RealSrcSpan rspan _) <- [replLocation]
  , let spanLoc = unpackFS $ srcSpanFile rspan
  , let edit = TextEdit (realSrcSpanToRange rspan) (T.pack replReplacement)
  ]

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

getCPPmodule :: Recorder (WithPriority Log) -> IdeState -> HscEnv -> FixityEnv -> FilePath -> IO (CPP AnnotatedModule)
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

  contents <- do
    mbContentsVFS <-
      runAction "Retrie.GetFileContents" state $ getFileContents nt
    case mbContentsVFS of
      Just contents -> return $ Rope.toText contents
      Nothing       -> T.decodeUtf8 <$> BS.readFile (fromNormalizedFilePath nt)
  if any (T.isPrefixOf "#if" . T.toLower) (T.lines contents)
    then parseCPP getParsedModule contents
    else do
      pm <- useOrFail state "Retrie.GetParsedModule" NoParse GetParsedModule nt
      NoCPP <$> transformA (fixAnns pm) (fix fixities)
