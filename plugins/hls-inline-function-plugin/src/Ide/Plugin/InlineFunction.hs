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
import           Control.Monad                     (forM, guard)
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Class         (lift)
import           Control.Monad.Trans.Except        (throwE)
import           Control.Monad.Trans.Maybe         (MaybeT (..), runMaybeT)
import qualified Data.Text.Utf16.Rope.Mixed        as Rope
import           Development.IDE                   (Action, IdeState,
                                                    filePathToUri', runAction,
                                                    toNormalizedFilePath', use)
import           Development.IDE.Core.FileStore    (getFileContents)
import           Development.IDE.Core.RuleTypes    (GetHieAst (..),
                                                    GhcSessionDeps (..),
                                                    TcModuleResult (..),
                                                    TypeCheck (..))
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
import           GHC.Generics                      (Generic)

import qualified Data.Map                          as M
import           Data.Maybe                        (catMaybes)
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
                                                    callSiteAt,
                                                    findAllCallSites,
                                                    findDefinition,
                                                    nameUnderCursor)
import           Ide.Plugin.InlineFunction.Rewrite (buildEdits, fixityEnvFor)
import           Ide.Plugin.InlineFunction.Util    (toRealSrcSpan)
import qualified Ide.Plugin.Resolve                as Resolve
import qualified Language.LSP.Protocol.Lens        as L

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
  -- find the binding in the defining module's renamed source (i.e. once all
  -- 'Name's have been uniquely resolved) and check it can be inlined
  definition <- MaybeT $ pure $ findDefinition (tmrRenamed defCheck) name
  let sites = findAllCallSites (tmrRenamed check) name (length definition.params)
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
resolveProvider
  :: Recorder (WithPriority Log)
  -> ResolveFunction IdeState InlineResolveData Method_CodeActionResolve
resolveProvider recorder state _plId ca uri (InlineResolveData pos scope) = do
  path <- getNormalizedFilePathE uri
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
  fileEdits <- forM targets $ \target -> do
    maybeInputs <- liftIO $ runAction "InlineFunction.resolve" state $
      runMaybeT $ rewriteInputs target
    case maybeInputs of
      -- files the session cannot load are skipped rather than failing
      -- the whole edit
      Nothing -> pure Nothing
      Just (source, check, env, contents) -> do
        let allSites =
              findAllCallSites (tmrRenamed check) cand.name (length cand.definition.params)
            sites = case scope of
              InlineAll    -> allSites
              InlineSingle -> callSiteAt pos allSites
        if null sites
          then pure Nothing
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
                -- failed with error
                logWith recorder Logger.Warning (LogBuildEditsFailed err)
                throwE (PluginInternalError ("buildEdits: " <> T.pack err))
              -- no call site was rewritten, so don't add imports either
              Right [] -> pure Nothing
              Right edits ->
                case importEdits
                       (tmrTypechecked defCheck)
                       (tmrTypechecked check)
                       source
                       contents
                       cand.definition.bodyRefs of
                  -- a binding the spliced body needs cannot be imported
                  -- here; inlining would not compile, so leave this file
                  -- unchanged
                  Nothing -> pure Nothing
                  Just importTextEdits ->
                    pure $
                      Just
                        ( fromNormalizedUri (filePathToUri' target)
                        , edits <> importTextEdits
                        )
  -- the resolve wrapper requires an edit on every resolved action, so
  -- "nothing to change" is an empty edit rather than a missing one
  pure $ ca & L.edit ?~ mkWorkspaceEdit (catMaybes fileEdits)

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
