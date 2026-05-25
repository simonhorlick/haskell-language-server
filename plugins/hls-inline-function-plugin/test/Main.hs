{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedRecordDot      #-}
{-# LANGUAGE OverloadedStrings        #-}

module Main ( main ) where

import           Data.Maybe                              (mapMaybe)
import qualified Data.Text                               as T
import           GHC.Paths                               (libdir)
import qualified Ide.Plugin.InlineFunction               as InlineFunction
import           Ide.Plugin.InlineFunction.Util          (addParens)
import           Language.Haskell.GHC.ExactPrint         (exactPrint,
                                                          makeDeltaAst)
import           Language.Haskell.GHC.ExactPrint.Parsers (parseExpr,
                                                          withDynFlags)
import qualified Language.LSP.Protocol.Types             as L
import           System.FilePath                         ((</>))
import           Test.Hls

main :: IO ()
main = defaultTestRunner test

plugin :: PluginTestDescriptor InlineFunction.Log
plugin = mkPluginTestDescriptor InlineFunction.descriptor "inline-function"

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-inline-function-plugin" </> "test" </> "testdata"

-- Find the unique code action with the given title.
pickAction
  :: HasCallStack
  => T.Text
  -> [L.Command L.|? L.CodeAction]
  -> Session L.CodeAction
pickAction title actions =
  let
    matches = filter (\a -> a._title == title) (codeActions actions)
  in case matches of
    [a] -> pure a
    xs  -> liftIO $ assertFailure $
             "Expected exactly one code action with title " <> show title
               <> ", got " <> show (length xs)

codeActions :: [L.Command L.|? L.CodeAction] -> [L.CodeAction]
codeActions xs =
  mapMaybe
    (\case
      InL _ -> Nothing
      InR r -> Just r)
    xs

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

-- | Parse @expr@, wrap it with 'addParens', and exactprint the result. The
-- parsed expression is relativised with 'makeDeltaAst' first, mirroring how
-- the plugin operates on delta-annotated source.
exactPrintAddParens :: String -> IO String
exactPrintAddParens expr = do
  parsed <- withDynFlags libdir (\dflags -> parseExpr dflags "<addParens-test>" expr)
  case parsed of
    Left _  -> assertFailure ("could not parse test expression: " <> expr)
    Right e -> pure (exactPrint (addParens (makeDeltaAst e)))

-- | Unit tests that exercise 'addParens' directly, without an LSP session.
unitTests :: TestTree
unitTests = testGroup "addParens" [
    testCase "wraps a compound expression in parentheses that exactprint renders" $ do
      printed <- exactPrintAddParens "f x"
      printed @?= "(f x)"
  , testCase "leaves an atomic expression unparenthesized" $ do
      printed <- exactPrintAddParens "x"
      printed @?= "x"
  ]

resolveTests :: TestTree
resolveTests = testGroup "resolve" [
    runTest "Inline top-level definition" "Inline foo" "TopLevel" (Position 6 7)
  , runTest "Inline constant" "Inline a" "Constant" (Position 6 10)
  , runTest "Rename variables that would be incorrectly captured after substitution" "Inline addOne" "Capture" (Position 6 11)
  , runTest "Doesn't rename shadowed identifier" "Inline idy" "Shadow" (Position 6 11)
  , runTest "Inlines an infix function correctly" "Inline add" "Infix" (Position 6 12)
  , runTest "Inlines let expression correctly" "Inline addOne" "Let" (Position 6 9)
  , runTest "Inlines parenthesized expression correctly" "Inline mul" "Parenthesis" (Position 6 9)
  , runTest "Inlines point free function correctly" "Inline trip" "Pointfree" (Position 11 7)
  , runTest "Parenthesizes the inlined body when the context requires it" "Inline addOne" "BodyParens" (Position 6 14)
  , runTest "Parenthesizes a function-application argument substituted into the body" "Inline combine" "ArgParens" (Position 9 15)
  , runTest "Inlines callsites in do-blocks correctly" "Inline foo" "DoBlock" (Position 5 11)
  -- Currently duplicates the argument expression at each occurrence. In order
  -- to retain exactly the same behaviour this could be pulled out into a let
  -- binding.
  , runTest "Duplicates the argument when a parameter is used multiple times" "Inline double" "DuplicateArg" (Position 8 10)
  , runTest "Offers inlining for type class methods" "Inline identity" "Class" (Position 6 13)
  , runTest "Offers inlining for let bindings" "Inline y" "Let2" (Position 5 5)
  , runTest "Handle capture for let bindings" "Inline x" "Let3" (Position 5 15)
  , runTest "Offers inlining for bindings in where clauses" "Inline foo" "Where" (Position 2 11)
  , runTest "Offers inlining for operators" "Inline */" "Operator" (Position 4 15)
  , runTest "Offers inlining for qualified names" "Inline foo" "Qualified" (Position 5 9)
  , runTest "Inlines a function that uses overloaded record fields" "Inline getName" "Overloaded" (Position 10 23)
  , runTest "Renames a where-bound name that would capture a body free variable" "Inline addY" "CaptureWhere" (Position 14 11)
  ]

actionTests :: TestTree
actionTests = testGroup "action" [
    runActionTest "Type signature offers no Inline action" "TopLevel" (Position 2 7) []
  , runActionTest "Variables offer no Inline action" "TopLevel" (Position 3 8) []
  , runActionTest "Offers inlining at definition" "Constant" (Position 3 0) ["Inline a"]
  , runActionTest "Recursive functions cannot be inlined" "Recursive" (Position 6 9) []
  , runActionTest "Functions consisting of guards cannot be inlined" "Guards" (Position 8 6) []
  , runActionTest "Pattern bindings cannot be inlined" "PatternBind" (Position 5 9) []
  , runActionTest "Bindings with multiple clauses cannot be inlined" "MultiClause" (Position 7 9) []
  , runActionTest "Imported names cannot be inlined" "Imported" (Position 5 14) []
  , runActionTest "Functions with no call sites offer no Inline action" "Uncalled" (Position 3 0) []
  -- TODO(simonhorlick): not yet implemented
  , runActionTest "Definitions imported from local modules do not offer inlining" "LocalImport" (Position 4 6) []
  , runActionTest "Does not offer inlining when a RecordWildCards binding in the body would capture an argument" "RecordWildCards" (Position 14 11) []
  , runActionTest "Does not offer inlining when there is a RecordWildCards binding in the arguments" "RecordWildCards2" (Position 15 11) []
  -- TODO(simonhorlick): not yet implemented
  , runActionTest "Does not offer inlining when a forall'd type variable in the body would be captured at the call site" "ImplicitForall" (Position 14 13) []
  ]

test :: TestTree
test = testGroup "inline-function" [
    -- self-contained tests that don't rely on the HLS
    unitTests
    -- tests that invoke the action
  , resolveTests
    -- tests that verify the code action is emitted
  , actionTests
  ]
