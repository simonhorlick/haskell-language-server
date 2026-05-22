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

import           Development.IDE               (IdeState)
import           Ide.Logger                    (Pretty (..), Recorder,
                                                WithPriority)
import           Ide.Types

import           Language.LSP.Protocol.Message
import           Language.LSP.Protocol.Types   as JL

import           Control.Monad.Trans.Class
import           Data.Aeson                    (FromJSON, ToJSON)
import           GHC.Generics                  (Generic)

data Log

instance Pretty Log where
    pretty = \case {}

-- |Plugin descriptor
descriptor :: Recorder (WithPriority Log) -> PluginId -> PluginDescriptor IdeState
descriptor recorder plId =
    (defaultPluginDescriptor plId "Provides a code action to inline functions")
        { pluginHandlers = mconcat
            [ mkPluginHandler SMethod_TextDocumentCodeAction (codeAction recorder)
            ]
        , pluginCommands = [inlineFunctionCommand]
        }

codeAction :: Recorder (WithPriority Log) -> PluginMethodHandler IdeState Method_TextDocumentCodeAction
codeAction _recorder _st _plId CodeActionParams{_textDocument,_range} = do
    pure $ InL []


-- | The command handler.
inlineFunctionCommand :: PluginCommand IdeState
inlineFunctionCommand =
  PluginCommand
    { commandId = importCommandId
    , commandDesc = "inlineFunctionCommand"
    , commandFunc = runInlineFunctionCommand
    }

importCommandId :: CommandId
importCommandId = "InlineFunctionCommand"

-- | The type of the parameters accepted by our command
newtype InlineFunctionCommandParams = InlineFunctionCommandParams WorkspaceEdit
  deriving (Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | The actual command handler
runInlineFunctionCommand :: CommandFunction IdeState InlineFunctionCommandParams
runInlineFunctionCommand _ _ (InlineFunctionCommandParams edit) = do
  -- This command simply triggers a workspace edit!
  _ <- lift $ pluginSendRequest SMethod_WorkspaceApplyEdit (ApplyWorkspaceEditParams Nothing edit) (\_ -> pure ())
  return $ InR JL.Null

