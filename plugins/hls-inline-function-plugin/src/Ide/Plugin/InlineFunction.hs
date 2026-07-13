{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DuplicateRecordFields #-}

{- |
Inline Function Plugin entry point.
-}
module Ide.Plugin.InlineFunction (
  descriptor,
  Log
  ) where

import           Control.Lens                      ((&), (?~), (^.))
import           Control.Monad                     (forM, guard, unless)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Class         (lift)
import           Control.Monad.Trans.Except        (ExceptT (..), runExceptT,
                                                    throwE)
import           Control.Monad.Trans.Maybe         (MaybeT (..), runMaybeT)
import qualified Data.Text.Utf16.Rope.Mixed        as Rope
import           Development.IDE                   (Action, IdeState,
                                                    filePathToUri', runAction,
                                                    toNormalizedFilePath', use)
import           Development.IDE.Core.FileStore    (getFileContents)
import           Development.IDE.Core.RuleTypes    (GetHieAst (..),
                                                    GhcSessionDeps (..),
                                                    TcModuleResult (..),
                                                    TypeCheck (..),
                                                    tmrModSummary)
import           Development.IDE.Core.Shake        (ShakeExtras (withHieDb),
                                                    getShakeExtras)
import           Development.IDE.GHC.ExactPrint    (GetAnnotatedParsedSource (..))
import qualified Development.IDE.GHC.ExactPrint    as E
import           Development.IDE.Types.HscEnvEq    (hscEnv)
import           Ide.Logger                        (Pretty (..), Recorder,
                                                    WithPriority, cmapWithPrio,
                                                    logWith)
import qualified Ide.Logger                        as Logger
import           Ide.Plugin.Error                  (PluginError (..),
                                                    getNormalizedFilePathE,
                                                    handleMaybe)
import           Ide.Types

import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types       as JL

import           Data.Aeson                        (FromJSON, ToJSON (toJSON))
import           Data.List                         (sortOn)
import           GHC.Generics                      (Generic)

import qualified Data.Map                          as M
import qualified Data.Set                          as S
import qualified Data.Text                         as T
import           Development.IDE.GHC.Compat
import qualified Development.IDE.GHC.Compat.Util   as Util
import           Development.IDE.Plugin.CodeAction (mkExactprintPluginDescriptor)
import           HieDb                             ((:.) (..))
import qualified HieDb
import           Ide.Plugin.InlineFunction.Imports (importEdits)
import           Ide.Plugin.InlineFunction.Resolve (BindingDef (..),
                                                    CursorSite (..),
                                                    InlineCandidate (..),
                                                    callSiteAt, clauseRefsFor,
                                                    findAllCallSites,
                                                    findDefinition,
                                                    nameUnderCursor,
                                                    selectClauseSites)
import           Ide.Plugin.InlineFunction.Rewrite (buildEdits,
                                                    editsTouchMangledLines,
                                                    fixityEnvFor)
import           Ide.Plugin.InlineFunction.Util    (toRealSrcSpan)
import qualified Ide.Plugin.Resolve                as Resolve
import qualified Language.LSP.Protocol.Lens        as L
import           Language.LSP.Server               (ProgressCancellable (Cancellable))
import           Retrie.Fixity                     (FixityEnv)
import           System.FilePath                   (takeFileName)

data Log
  = LogExactPrint E.Log
  | forall a. Pretty a => LogResolve a
  | LogBuildEditsFailed String

instance Pretty Log where
  pretty = \case
    LogExactPrint l       -> pretty l
    LogResolve l          -> pretty l
    LogBuildEditsFailed e -> "buildEdits failed:" Logger.<+> pretty e

-- | Which call sites the action rewrites: every one in the project, or
-- just the one under the cursor.
data InlineScope = InlineAll | InlineSingle
  deriving stock    (Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Data passed back from the client as part of the resolve stage.
data InlineResolveData = InlineResolveData
  { position :: Position
  , scope    :: InlineScope
  }
  deriving stock    (Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Plugin descriptor
descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
  -- Provides access to GetAnnotatedParsedSource
  mkExactprintPluginDescriptor (cmapWithPrio LogExactPrint recorder) $
    (defaultPluginDescriptor plId "Provides a code action to inline functions")
      { pluginHandlers =
          Resolve.mkCodeActionHandlerWithResolve
            (cmapWithPrio LogResolve recorder)
            codeAction
            (resolveProvider recorder)
      }

-- | Locate the file that defines @name@: the requesting file itself for
-- names bound in its module, otherwise the file the name's definition span
-- points into. Names from external packages either have no usable span or
-- resolve to a file outside the project, so the subsequent 'TypeCheck'
-- lookup fails and no action is offered for them.
definitionFile
  :: TcModuleResult
  -> NormalizedFilePath
  -> Name
  -> Maybe NormalizedFilePath
definitionFile check path name
  | nameIsLocalOrFrom (tcg_mod (tmrTypechecked check)) name = Just path
  | otherwise = do
      sp <- toRealSrcSpan (nameSrcSpan name)
      pure $ toNormalizedFilePath' (Util.unpackFS (srcSpanFile sp))

-- | Find the inline candidate under the cursor, together with the file its
-- definition lives in and whether the cursor stood on a use of the function
-- or on its definition. The definition may sit in a different project module
-- than the requesting file; call sites are collected from the requesting
-- file.
findCandidate
  :: NormalizedFilePath
  -> Position
  -> Action (Maybe (InlineCandidate, NormalizedFilePath, CursorSite))
findCandidate path pos = runMaybeT $ do
  ast          <- MaybeT $ use GetHieAst path
  check        <- MaybeT $ use TypeCheck path
  (name, site) <- MaybeT $ pure $ nameUnderCursor ast pos
  defPath      <- MaybeT $ pure $ definitionFile check path name
  defCheck <-
    if defPath == path
      then pure check
      else MaybeT $ use TypeCheck defPath
  -- class-method calls dispatch through the instance dictionary, so no
  -- single implementation (default or instance) can be spliced into a
  -- call site without changing the program's meaning
  guard $ not (isClassMethod defCheck name)
  -- a body defined in a QuasiQuotes module may contain a quasi-quote,
  -- whose syntax only parses where that extension is enabled. We append
  -- the imports a spliced body needs but cannot enable an extension at
  -- the target, so splicing such a body would leave the target
  -- unparseable. Refuse the whole module rather than inspect each body.
  guard $ not (xopt QuasiQuotes (ms_hspp_opts (tmrModSummary defCheck)))
  -- find the binding in the defining module's renamed source (i.e. once all
  -- 'Name's have been uniquely resolved) and check it can be inlined
  definition <- MaybeT $ pure $ findDefinition (tmrRenamed defCheck) name
  -- extension-gated syntax in the definition (a '..' wildcard, a \case,
  -- a multi-way if, a view pattern) travels with the splice intact and
  -- only parses where its extension is on. Like QuasiQuotes we cannot
  -- enable extensions at the target, but these are common enough that
  -- refusing every body from such a module would be too blunt: refuse
  -- only definitions that actually carry the syntax, and only where the
  -- module lacks the extension. This guards the requesting module; each
  -- further rewrite target is checked the same way in 'rewriteTarget'.
  guard $ spliceableInto definition (tmrModSummary check)
  let sites = selectClauseSites definition $
        findAllCallSites (tmrRenamed check) name definition.arity
  -- omit the candidate entirely when there is nothing to rewrite
  guard $ not (null sites)
  pure
    ( InlineCandidate
        { name       = name
        , definition = definition
        , sites      = sites
        }
    , defPath
    , site
    )

-- | Whether a module compiled with the given summary parses every
-- extension-gated syntax form the definition's splice carries. The
-- plugin can append imports at a target but not language pragmas, so a
-- module failing this check must not be rewritten.
spliceableInto :: BindingDef -> ModSummary -> Bool
spliceableInto definition ms =
  all (\ext -> xopt ext (ms_hspp_opts ms)) definition.neededExts

-- | True when @name@ is a type class method, per the defining module's
-- type environment. Covers both default methods and instance methods: a
-- use of either resolves to the class-op 'Name'.
isClassMethod :: TcModuleResult -> Name -> Bool
isClassMethod check name =
  case lookupNameEnv (tcg_type_env (tmrTypechecked check)) name of
    Just (AnId ident) -> isClassOpId ident
    _                 -> False

-- | For the given cursor position, determine if there is a function here that
-- could be inlined. If so, provide the details to display to the user. At a
-- use site the single-site variant is offered alongside inline-all, provided
-- the use is a rewriteable call site (a reference in operator position, say,
-- is a use but not a site); at the definition only inline-all makes sense.
codeAction :: PluginMethodHandler IdeState Method_TextDocumentCodeAction
codeAction state _plId CodeActionParams{_textDocument, _range} = do
  let uri = _textDocument ^. L.uri
      pos = _range ^. L.start
  path <- getNormalizedFilePathE uri
  candidate <- liftIO $ runAction "InlineFunction.codeAction" state $
    findCandidate path pos
  pure $
    InL $ case candidate of
      Nothing -> []
      Just (cand, _, site) ->
        [InR (mkAction cand pos InlineAll)]
          <> [ InR (mkAction cand pos InlineSingle)
             | site == AtUseSite
             , not (null (callSiteAt pos cand.sites))
             ]

-- | Creates a 'CodeAction' for the client to display.
mkAction :: InlineCandidate -> Position -> InlineScope -> CodeAction
mkAction cand pos scope =
  CodeAction
    { _title       = title
    , _kind        = Just CodeActionKind_RefactorInline
    , _diagnostics = Nothing
    , _isPreferred = Nothing
    , _disabled    = Nothing
    , _edit        = Nothing
    , _command     = Nothing
    , _data_       = Just (toJSON (InlineResolveData pos scope))
    }
  where
    fname = T.pack (occNameString (nameOccName cand.name))
    title = case scope of
      InlineAll    -> "Inline " <> fname
      InlineSingle -> "Inline " <> fname <> " at this use site"

-- | We receive a resolve request when the user has selected a code action in
-- the UI. The client will pass back the 'InlineResolveData' we provided earlier
-- so we can figure out exactly which function should be inlined and compute
-- the appropriate diff to apply.
--
-- The whole computation runs under a cancellable progress session: inlining
-- every use typechecks each file that references the function, which can
-- take a while on a large project. A target file that cannot be rewritten
-- does not abort the rest: the surviving edits are applied and the files
-- left out are reported in a warning notification. Only an error on a
-- single-site inline -- whose one target is the whole action -- fails the
-- resolve itself.
resolveProvider
  :: Recorder (WithPriority Log)
  -> ResolveFunction IdeState InlineResolveData Method_CodeActionResolve
resolveProvider recorder state _plId ca uri (InlineResolveData pos scope) = do
  path <- getNormalizedFilePathE uri
  ExceptT $ pluginWithIndefiniteProgress (ca ^. L.title) Nothing Cancellable $
    \updateProgress -> runExceptT $ do
      maybeResult <- liftIO $ runAction "InlineFunction.resolve" state $ runMaybeT $ do
        (cand, defPath, _) <- MaybeT $ findCandidate path pos
        -- the rewrite template is constructed from the defining module
        def <- rewriteInputs defPath
        pure (cand, defPath, def)
      (cand, defPath, (defSource, defCheck, defEnv, _)) <-
        handleMaybe
          (PluginInternalError "inline candidate no longer resolves")
          maybeResult
      -- Inlining every use rewrites all project files that reference the
      -- function: the requesting file, the defining file, and whatever other
      -- files the hiedb reference index knows about. Inlining a single use
      -- only ever touches the requesting file.
      targets <- case scope of
        InlineSingle -> pure [path]
        InlineAll -> do
          refFiles <- liftIO $ referencingFiles state cand.name
          pure $ S.toList (S.fromList (path : defPath : refFiles))
      defFixities <- liftIO $
        fixityEnvFor defEnv (tmrTypechecked defCheck) (tmrRenamed defCheck)
      let total = length targets
      outcomes <- forM (zip [1 :: Int ..] targets) $ \(i, target) -> do
        lift $ updateProgress $ T.pack $
          show i <> "/" <> show total <> " "
            <> takeFileName (fromNormalizedFilePath target)
        lift $ rewriteTarget recorder state pos scope cand (defSource, defCheck)
          defFixities target
      let fileEdits = [edit | TargetEdited edit <- outcomes]
          reported  = [ (t, reason)
                      | (t, outcome) <- zip targets outcomes
                      , reason <- case outcome of
                          TargetNotRewritable reason -> [reason]
                          TargetFailed reason        -> [reason]
                          _                          -> []
                      ]
      -- a single-site inline has exactly one target, so an error there
      -- fails the whole action rather than producing an empty edit
      case (scope, [reason | TargetFailed reason <- outcomes]) of
        (InlineSingle, reason : _) ->
          throwE (PluginInternalError (ca ^. L.title <> ": " <> reason))
        _ -> pure ()
      -- per-file failures do not abort an inline-all edit: apply what
      -- succeeded and tell the user what was left out
      unless (null reported) $
        lift $ pluginSendNotification SMethod_WindowShowMessage $
          ShowMessageParams MessageType_Warning $ T.unlines $
            (ca ^. L.title <> ": some files were not rewritten:")
              : [ "- " <> T.pack (fromNormalizedFilePath t) <> ": " <> reason
                | (t, reason) <- reported
                ]
      -- the resolve wrapper requires an edit on every resolved action, so
      -- "nothing to change" is an empty edit rather than a missing one
      pure $ ca & L.edit ?~ mkWorkspaceEdit fileEdits

-- | What rewriting one target file produced: its edit, nothing (no call
-- sites to rewrite), or the reason no edit was produced.
--
-- 'TargetNotRewritable' is an expected outcome -- the file has call sites
-- but inlining them would not compile -- and is only ever reported as a
-- warning; resolve must stay total for it because clients without resolve
-- support resolve every offered action eagerly, where one failure would
-- take down the whole code-action menu. 'TargetFailed' is an error.
data TargetOutcome
  = TargetEdited (Uri, [TextEdit])
  | TargetSkipped
  | TargetNotRewritable T.Text
  | TargetFailed T.Text

-- | Rewrite the call sites of one target file. A failure only concerns
-- this target; the caller decides whether it aborts the whole action.
rewriteTarget
  :: Recorder (WithPriority Log)
  -> IdeState
  -> Position
  -> InlineScope
  -> InlineCandidate
  -> (ParsedSource, TcModuleResult)
  -- ^ Annotated source and typecheck result of the defining module.
  -> FixityEnv
  -- ^ Fixities of the operators the defining module uses.
  -> NormalizedFilePath
  -> HandlerM Config TargetOutcome
rewriteTarget recorder state pos scope cand (defSource, defCheck) defFixities target = do
  maybeInputs <- liftIO $ runAction "InlineFunction.resolve" state $
    runMaybeT $ rewriteInputs target
  case maybeInputs of
    Nothing -> pure $ TargetFailed "the module could not be loaded"
    Just (source, check, env, contents) -> do
      let allSites = selectClauseSites cand.definition $
            findAllCallSites (tmrRenamed check) cand.name cand.definition.arity
          sites = case scope of
            InlineAll    -> allSites
            InlineSingle -> callSiteAt pos allSites
      if null sites
        then pure TargetSkipped
        -- the requesting module was vetted when the action was offered
        -- ('findCandidate'), but inline-all reaches further modules whose
        -- extension sets differ; a target that cannot parse the spliced
        -- syntax must be left unchanged
        else if not (spliceableInto cand.definition (tmrModSummary check))
          then pure $ TargetNotRewritable
            "the inlined code needs a language extension this module does not enable"
        else do
          -- operators spliced in with the body come from the defining
          -- module, the ones around the call site from the target
          fixities <- liftIO $
            (defFixities <>)
              <$> fixityEnvFor env (tmrTypechecked check) (tmrRenamed check)
          editsResult <- liftIO $
            buildEdits
              fixities
              (defSource, tmrRenamed defCheck)
              (source, tmrRenamed check)
              cand{sites = sites}
          case editsResult of
            Left err -> do
              logWith recorder Logger.Warning (LogBuildEditsFailed err)
              pure $ TargetFailed ("the rewrite failed: " <> T.pack err)
            -- no call site was rewritten, so don't add imports either
            Right (_, []) -> pure TargetSkipped
            -- the edits' line ranges refer to the exact-printed
            -- (preprocessed) module; one that touches a line the
            -- preprocessor rewrote -- a CPP directive or a dead '#if'
            -- branch, blank in the print -- would splice fragments into
            -- that region of the real document, so the file must be
            -- left unchanged
            Right (printed, edits)
              | editsTouchMangledLines printed contents edits ->
                  pure $ TargetNotRewritable
                    "the rewrite would edit lines the preprocessor changed"
              | otherwise ->
              case importEdits
                     (tmrTypechecked defCheck)
                     (tmrTypechecked check)
                     source
                     contents
                     (clauseRefsFor cand.definition sites) of
                -- a binding the spliced body needs cannot be imported
                -- here; inlining would not compile, so the file must be
                -- left unchanged
                Nothing ->
                  pure $ TargetNotRewritable
                    "the inlined body needs a binding that cannot be imported here"
                Just importTextEdits ->
                  -- ascending by position: some clients (lsp-test among
                  -- them) turn the edit list into sequential didChange
                  -- events and rely on that order, and the import edit
                  -- would otherwise come last while sitting first
                  pure $ TargetEdited
                    ( fromNormalizedUri (filePathToUri' target)
                    , sortOn (\e -> e._range._start) (edits <> importTextEdits)
                    )

-- | Files the hiedb reference index knows use @name@. The index only covers
-- modules that have already been compiled, so it can lag behind the state of
-- the project; the caller always adds the requesting and defining files.
referencingFiles :: IdeState -> Name -> IO [NormalizedFilePath]
referencingFiles state name = do
  extras <- runAction "InlineFunction.hiedb" state getShakeExtras
  case nameModule_maybe name of
    Nothing -> pure []
    Just m -> do
      rows <- withHieDb extras $ \hieDb ->
        HieDb.findReferences
          hieDb
          True
          (nameOccName name)
          (Just $ moduleName m)
          (Just $ moduleUnit m)
          []
      pure
        [ toNormalizedFilePath' file
        | (_ :. info) <- rows
        , Just file <- [HieDb.modInfoSrcFile info]
        ]

-- | The per-module inputs the rewrite stage needs. 'GhcSessionDeps' is used
-- so already-loaded interfaces are reused when we look up imported fixities
-- (mirrors hls-explicit-fixity-plugin).
rewriteInputs
  :: NormalizedFilePath
  -> MaybeT Action (ParsedSource, TcModuleResult, HscEnv, T.Text)
rewriteInputs path = do
  source  <- MaybeT $ use GetAnnotatedParsedSource path
  check   <- MaybeT $ use TypeCheck path
  session <- MaybeT $ use GhcSessionDeps path
  -- the text is only consulted for the import insertion point's
  -- pragma-scanning fallback; a file not held in memory falls back to
  -- empty, which degrades that fallback to the top of the file
  contents <- lift $ maybe "" Rope.toText <$> getFileContents path
  pure (source, check, hscEnv session, contents)

-- | Construct a 'WorkspaceEdit' from the per-file 'TextEdit's.
mkWorkspaceEdit :: [(Uri, [TextEdit])] -> WorkspaceEdit
mkWorkspaceEdit fileEdits =
  WorkspaceEdit
    { _changes           = Just (M.fromList fileEdits)
    , _documentChanges   = Nothing
    , _changeAnnotations = Nothing
    }
