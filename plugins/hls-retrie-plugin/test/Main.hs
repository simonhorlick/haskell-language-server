{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PartialTypeSignatures    #-}

module Main (main) where

import qualified Data.Map                          as M
import           Data.Text                         (Text)
import qualified Development.IDE.GHC.ExactPrint    as ExactPrint
import qualified Development.IDE.Plugin.CodeAction as Refactor
import           Ide.Logger
import           Ide.Plugin.Config
import qualified Ide.Plugin.Retrie                 as Retrie
import           System.FilePath
import           Test.Hls

data LogWrap
    = RetrieLog Retrie.Log
    | ExactPrintLog ExactPrint.Log

instance Pretty LogWrap where
    pretty = \case
        RetrieLog msg -> pretty msg
        ExactPrintLog msg -> pretty msg

main :: IO ()
main = defaultTestRunner tests

retriePlugin :: PluginTestDescriptor LogWrap
retriePlugin =  mkPluginTestDescriptor (Retrie.descriptor . cmapWithPrio RetrieLog) "retrie"

refactorPlugin :: PluginTestDescriptor LogWrap
refactorPlugin = mkPluginTestDescriptor (Refactor.iePluginDescriptor  . cmapWithPrio ExactPrintLog) "refactor"

tests :: TestTree
tests =
  testGroup "Retrie"
    [ inlineThisTests
    ]

inlineThisTests :: TestTree
inlineThisTests =
  testGroup "Inline this"
    [ testGroup "provider"
      [ testProvider "lhs" "Identity" 7 0 ["Unfold f", "Unfold f in current file"]
      , testProvider "identifier" "Identity" 7 6 ["Inline e"]
      , testProvider "imported identifier" "Imported" 4 6 ["Inline e"]
      , testProvider "nested where" "Where" 3 6 ["Inline e"]
      , testProvider "nested let" "NestedLet" 6 12 ["Inline identity"]
      , testProvider "class member" "Class" 5 16 []
      , testProvider "operator" "Operator" 4 16 ["Inline */"]
      ]
    , testGroup "command"
      [ testCommand "top level function" "Identity" 7 6
      , testCommand "top level function in another file" "Imported" 4 6
      , testCommand "nested where function" "Where" 3 6
      , testCommand "nested let function" "NestedLet" 6 12
      , testCommand "operator" "Operator" 4 16
      , testCommand "uses imported fixities" "OpChain" 10 4
      , testCommand "expression has lower precedence" "BodyParens" 6 8
      , testCommand "expression is let" "Let" 6 4
      , testCommand "partially applied function" "Partial" 7 8
      , testCommand "lambda with a pattern" "PatternLambda" 4 4
      , testCommand "let capture" "LetCapture" 5 36
      , testCommand "infix with extra arguments" "InfixExtraArg" 8 8
      , testCommand "list use" "ListLiteral" 6 5
      , testCommand "parens around a right-associative body on the LHS" "RightAssocOp" 10 4
      , testCommand "where to let" "Where2" 6 6
      , testCommand "where to let annotation" "Where3" 7 6
      , testCommand "substitutes arguments into the where clause" "Where5" 6 4
      , testCommand "lambda outside the let for a partially applied function" "PartialLet" 8 8
      , testCommand "rename RecordWildCards binding" "RecordWildCards" 14 6
      , testCommand "refuses a site where a where-binding would capture a free variable" "CaptureWhere" 9 6
      , testCommand "refuses a capturing site when the capture is via the argument" "ArgRename" 11 6
      , testCommand "qualified" "Qualified" 5 6
      , testCommand "multi-line body at a deeper call site" "LayoutDeep" 13 8
      ]
    , dispatchTest $ testGroup "dispatch"
      [ testCommand "dispatch multi clause" "MultiClause" 7 4
      , testCommand "transfer guards to case" "Guards" 9 4
      , testCommand "multi parameter dispatch" "MultiClauseTuple" 7 6
      , testCommand "multi clause partial" "MultiClausePartial" 8 8
      , testCommand "dispatch wildcard argument" "RecordWildCards2" 14 6
      , testCommand "dispatch wildcard construction from parameters" "RecordWildCardsConstruct" 10 4
      ]
    ]
-- | Dispatch-preserving rewrites need the GHC >= 9.12 exact-print
-- annotation API; older compilers fall back to per-clause rewrites and
-- these goldens fail.
dispatchTest :: TestTree -> TestTree
#if __GLASGOW_HASKELL__ >= 912
dispatchTest = id
#else
dispatchTest = expectFailBecause "dispatch needs GHC >= 9.12"
#endif

testProvider :: TestName -> FilePath -> UInt -> UInt -> [Text] -> TestTree
testProvider title file line row expected = testCase title $ runWithRetrie $ do
    adoc <- openDoc (file <.> "hs") "haskell"
    _ <- waitForTypecheck adoc
    let position = Position line row
    codeActions <- getCodeActions adoc $ Range position position
    liftIO $ map codeActionTitle codeActions @?= map Just expected

testCommand :: TestName -> FilePath -> UInt -> UInt -> TestTree
testCommand title file row col = goldenWithRetrie title file $ \adoc -> do
    _ <- waitForTypecheck adoc
    let p = Position row col
    codeActions <- getCodeActions adoc $ Range p p
    case codeActions of
        [InR ca] -> resolveAndExecuteCodeAction ca
        cas -> liftIO . assertFailure $ "One code action expected, got " <> show (length cas)

codeActionTitle :: (Command |? CodeAction) -> Maybe Text
codeActionTitle (InR CodeAction {_title}) = Just _title
codeActionTitle _                         = Nothing

goldenWithRetrie :: TestName -> FilePath -> (TextDocumentIdentifier -> Session ()) -> TestTree
goldenWithRetrie title path act =
    goldenWithHaskellAndCaps (def { plugins = M.singleton "retrie" def }) codeActionResolveCaps testPlugins title testDataDir path "expected" "hs" act

runWithRetrie :: Session a -> IO a
runWithRetrie = runSessionWithTestConfig def
    { testDirLocation = Left testDataDir
    , testConfigCaps = codeActionResolveCaps
    , testPluginDescriptor = testPlugins
    } . const

testPlugins :: PluginTestDescriptor LogWrap
testPlugins =
    retriePlugin <>
    refactorPlugin  -- needed for the GetAnnotatedParsedSource rule

testDataDir :: FilePath
testDataDir = "plugins" </> "hls-retrie-plugin" </> "test" </> "testdata"
