{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# OPTIONS_GHC -Wwarn #-}

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
                                                    TcModuleResult (..),
                                                    TypeCheck (..))
import           Development.IDE.GHC.ExactPrint    (GetAnnotatedParsedSource (..))
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
import           Ide.Plugin.InlineFunction.Resolve (InlineCandidate (name),
                                                    findInlineCandidate)
import           Ide.Plugin.InlineFunction.Rewrite (buildEdits)
import qualified Ide.Plugin.Resolve                as Resolve
import qualified Language.LSP.Protocol.Lens        as L

data Log = forall a. Pretty a => LogResolve a | LogBuildEditsFailed String

instance Pretty Log where
    pretty = \case
      LogResolve l          -> pretty l
      LogBuildEditsFailed e -> "buildEdits failed:" Logger.<+> pretty e

-- | Data passed back from the client as part of the resolve stage.
newtype InlineResolveData = InlineResolveData { irdPos :: Position }
    deriving stock    (Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | Plugin descriptor
descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
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
        maybeAst <- use GetHieAst path
        maybeTypecheck <- use TypeCheck path
        pure $ do
            ast <- maybeAst
            tcm <- maybeTypecheck
            findInlineCandidate ast (tmrRenamed tcm) pos
    pure $ InL $ case candidate of
        Nothing   -> []
        Just cand -> [InR (mkAction cand pos)]

-- | Creates a 'CodeAction' for the client to display.
mkAction :: InlineCandidate -> Position -> CodeAction
mkAction cand pos = CodeAction
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
    title = "Inline " <> T.pack (occNameString (nameOccName (name cand)))

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
        maybeAst <- use GetHieAst path
        maybeTypecheck <- use TypeCheck path
        maybeParsedSource  <- use GetAnnotatedParsedSource path
        pure $ do
            ast <- maybeAst
            tcm <- maybeTypecheck
            ps  <- maybeParsedSource
            let dflags = ms_hspp_opts (pm_mod_summary (tmrParsed tcm))
            cand <- findInlineCandidate ast (tmrRenamed tcm) pos
            pure (cand, ps, dflags)
    (cand, ps, dflags) <-
        handleMaybe (PluginInternalError "inline candidate no longer resolves") maybeResult
    case buildEdits dflags ps cand of
        Left err -> do
            logWith recorder Logger.Warning (LogBuildEditsFailed err)
            throwE (PluginInternalError ("buildEdits: " <> T.pack err))
        Right edits ->
            pure (ca & L.edit ?~ mkWorkspaceEdit uri edits)

-- | Construct a 'WorkspaceEdit' from a bunch of 'TextEdits'.
mkWorkspaceEdit :: Uri -> [TextEdit] -> WorkspaceEdit
mkWorkspaceEdit uri edits = WorkspaceEdit
    { _changes           = Just (M.singleton uri edits)
    , _documentChanges   = Nothing
    , _changeAnnotations = Nothing
    }
