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

runActionTest :: TestName -> FilePath -> Position -> [T.Text] -> TestTree
runActionTest title file pos expected =
    testCase title $ runInlineSession $ do
        doc     <- openDoc (file ++ ".hs") "haskell"
        _       <- waitForBuildQueue
        actions <- getCodeActions doc (L.Range pos pos)
        liftIO $ filter ("Inline " `T.isPrefixOf`) (actionTitles actions) @?= expected

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
    , runTest "Inline constant" "Inline a" "Constant" (Position 6 10)
    , runTest "Rename variables that would be incorrectly captured after substitution" "Inline a" "Capture" (Position 6 11)
    , runTest "Inlines an infix function correctly" "Inline add" "Infix" (Position 6 12)
    ]
  , testGroup "action" [
      runActionTest "Type signature offers no Inline action" "TopLevelCall" (Position 2 7) []
    , runActionTest "Variables offer no Inline action" "TopLevelCall" (Position 3 8) []
    , runActionTest "Offers inlining at definition" "Constant" (Position 3 0) ["Inline a"]
    , runActionTest "Recursive functions cannot be inlined" "Recursive" (Position 6 9) []
    , runActionTest "Functions consisting of guards cannot be inlined" "Guards" (Position 8 6) []
    , runActionTest "Pattern bindings cannot be inlined" "PatternBind" (Position 5 9) []
    , runActionTest "Bindings with multiple clauses cannot be inlined" "MultiClause" (Position 7 9) []
    , runActionTest "Imported names cannot be inlined" "Imported" (Position 5 14) []
    ]
  ]
