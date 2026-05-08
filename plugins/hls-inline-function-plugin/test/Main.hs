{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings        #-}

module Main ( main ) where

import qualified Data.Text                   as T
import qualified Ide.Plugin.InlineFunction   as InlineFunction
import qualified Language.LSP.Protocol.Types as L
import           System.FilePath             ((</>))
import           Test.Hls

main :: IO ()
main = defaultTestRunner test

plugin :: PluginTestDescriptor InlineFunction.Log
plugin = mkPluginTestDescriptor InlineFunction.descriptor "inline-function"

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-inline-function-plugin" </> "test" </> "testdata"

-- Find the first code action with the given title.
pickAction :: T.Text -> [L.Command L.|? L.CodeAction] -> Session L.CodeAction
pickAction title actions =
  let matches = [a | L.InR a@L.CodeAction{_title = t} <- actions, t == title]
  in case matches of
    []    -> liftIO $ assertFailure $ "Action " ++ show title ++ " not found"
    (a:_) -> pure a

-- | Attempt an inline action at the given position. Compare the result against
-- the respective X.expected.hs file.
runTest :: TestName -> T.Text -> FilePath -> Position -> TestTree
runTest title action file pos =
    goldenWithHaskellDoc def plugin title testDataDir file "expected" "hs" $ \doc -> do
        _       <- waitForBuildQueue
        actions <- getCodeActions doc (L.Range pos pos)
        action  <- pickAction action actions
        executeCodeAction action

actionTitles :: [L.Command L.|? L.CodeAction] -> [T.Text]
actionTitles xs = [t | L.InR L.CodeAction{_title = t} <- xs]

runActionTest :: TestName -> FilePath -> Position -> TestTree
runActionTest title file pos =
    testCase title $ runInlineSession $ do
        doc     <- openDoc (file ++ ".hs") "haskell"
        _       <- waitForBuildQueue
        actions <- getCodeActions doc (L.Range pos pos)
        liftIO $ filter ("Inline " `T.isPrefixOf`) (actionTitles actions) @?= []

runInlineSession :: Session a -> IO a
runInlineSession =
    runSessionWithTestConfig def
        { testDirLocation      = Left testDataDir
        , testPluginDescriptor = plugin
        , testConfigCaps       = codeActionNoResolveCaps
        }
        . const

test :: TestTree
test = testGroup "inline-function" [
    testGroup "resolve" [
      runTest "Inline top-level definition" "Inline foo" "TopLevelCall" (Position 6 7)
    ]
  , testGroup "action" [
      runActionTest "Type signature offers no Inline action" "TopLevelCall" (Position 2 7)
    ]
  ]
