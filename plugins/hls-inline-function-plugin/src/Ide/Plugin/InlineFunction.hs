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
import           Control.Monad.IO.Class            (liftIO)
import           Control.Monad.Trans.Except        (throwE)
import           Development.IDE                   (IdeState, runAction, use)
import           Development.IDE.Core.RuleTypes    (GetHieAst (..),
                                                    GhcSessionDeps (..),
                                                    TcModuleResult (..),
                                                    TypeCheck (..))
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
import qualified Data.Text                         as T
import           Development.IDE.GHC.Compat
import           Development.IDE.Plugin.CodeAction (mkExactprintPluginDescriptor)
import           Ide.Plugin.InlineFunction.Resolve (InlineCandidate (..),
                                                    findInlineCandidate)
import           Ide.Plugin.InlineFunction.Rewrite (buildEdits, fixityEnvFor)
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

-- | Data passed back from the client as part of the resolve stage.
newtype InlineResolveData = InlineResolveData { position :: Position }
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

-- | For the given cursor position, determine if there is a function here that
-- could be inlined. If so, provide the details to display to the user.
codeAction :: PluginMethodHandler IdeState Method_TextDocumentCodeAction
codeAction state _plId CodeActionParams{_textDocument, _range} = do
  let uri = _textDocument ^. L.uri
      pos = _range ^. L.start
  path <- getNormalizedFilePathE uri
  candidate <- liftIO $ runAction "InlineFunction.codeAction" state $ do
    maybeAst   <- use GetHieAst path
    maybeCheck <- use TypeCheck path
    pure $ do
      ast   <- maybeAst
      check <- maybeCheck
      findInlineCandidate ast (tmrRenamed check) pos
  pure $
    InL $ case candidate of
      Nothing   -> []
      Just cand -> [InR (mkAction cand pos)]

-- | Creates a 'CodeAction' for the client to display.
mkAction :: InlineCandidate -> Position -> CodeAction
mkAction cand pos =
  CodeAction
    { _title       = title
    , _kind        = Just CodeActionKind_RefactorInline
    , _diagnostics = Nothing
    , _isPreferred = Nothing
    , _disabled    = Nothing
    , _edit        = Nothing
    , _command     = Nothing
    , _data_       = Just (toJSON (InlineResolveData pos))
    }
  where
    title = "Inline " <> T.pack (occNameString (nameOccName cand.name))

-- | We receive a resolve request when the user has selected a code action in
-- the UI. The client will pass back the 'InlineResolveData' we provided earlier
-- so we can figure out exactly which function should be inlined and compute
-- the appropriate diff to apply.
resolveProvider
  :: Recorder (WithPriority Log)
  -> ResolveFunction IdeState InlineResolveData Method_CodeActionResolve
resolveProvider recorder state _plId ca uri (InlineResolveData pos) = do
  path <- getNormalizedFilePathE uri
  maybeResult <- liftIO $ runAction "InlineFunction.resolve" state $ do
    maybeAst     <- use GetHieAst path
    maybeCheck   <- use TypeCheck path
    maybeSource  <- use GetAnnotatedParsedSource path
    -- GhcSessionDeps so already-loaded interfaces are reused when we look
    -- up imported fixities below (mirrors hls-explicit-fixity-plugin).
    maybeSession <- use GhcSessionDeps path
    pure $ do
      ast     <- maybeAst
      check   <- maybeCheck
      source  <- maybeSource
      session <- maybeSession
      -- find candidates in the renamed source (i.e. once all 'Name's have
      -- been uniquely resolved)
      cand <- findInlineCandidate ast (tmrRenamed check) pos
      pure (cand, source, hscEnv session, check)
  (cand, source, env, check) <-
    handleMaybe
      (PluginInternalError "inline candidate no longer resolves")
      maybeResult
  fixities    <- liftIO $ fixityEnvFor env (tmrTypechecked check) (tmrRenamed check)
  editsResult <- liftIO (buildEdits fixities source (tmrRenamed check) cand)
  case editsResult of
    Left err -> do
      -- failed with error
      logWith recorder Logger.Warning (LogBuildEditsFailed err)
      throwE (PluginInternalError ("buildEdits: " <> T.pack err))
    Right edits ->
      pure $
        ca & L.edit ?~ mkWorkspaceEdit uri edits

-- | Construct a 'WorkspaceEdit' from a bunch of 'TextEdits'.
mkWorkspaceEdit :: Uri -> [TextEdit] -> WorkspaceEdit
mkWorkspaceEdit uri edits =
  WorkspaceEdit
    { _changes           = Just (M.singleton uri edits)
    , _documentChanges   = Nothing
    , _changeAnnotations = Nothing
    }
