{-# LANGUAGE CPP                      #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PartialTypeSignatures    #-}

module Main (main) where

import qualified Data.Map                          as M
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import qualified Development.IDE.GHC.ExactPrint    as ExactPrint
import qualified Development.IDE.Plugin.CodeAction as Refactor
import           Development.IDE.Test              (referenceReady)
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
      [ testProvider "lhs" "Identity" 7 0 ["Inline f everywhere"]
      , testProvider "identifier" "Identity" 7 6 ["Inline e", "Inline e everywhere"]
      , testProvider "where binder" "Where" 5 4 ["Inline e everywhere"]
      , testProvider "let binder" "NestedLet" 5 8 ["Inline identity everywhere"]
      , testProvider "imported identifier" "Imported" 4 6 ["Inline e", "Inline e everywhere"]
      , testProvider "nested where" "Where" 3 6 ["Inline e", "Inline e everywhere"]
      , testProvider "nested let" "NestedLet" 6 12 ["Inline identity", "Inline identity everywhere"]
      , testProvider "class member" "Class" 5 16 []
      , testProvider "imported constructor in an expression" "ConUse" 5 6 []
      , testProvider "imported constructor in a pattern" "ConUse" 9 2 []
      , testProvider "parameter" "NotInlinable" 5 12 []
      , testProvider "record selector" "NotInlinable" 5 6 []
      , testProvider "imported record selector" "ImportedNotInlinable" 5 6 []
      , testProvider "imported class method" "ImportedNotInlinable" 8 6 []
      , testProvider "pattern-bound variable" "NotInlinable" 5 16 []
      , testProvider "where binder beside non-inlinables" "NotInlinable" 5 20 ["Inline y", "Inline y everywhere"]
      , testProvider "operator" "Operator" 4 16 ["Inline */", "Inline */ everywhere"]
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
      , testCommand "rename let binding" "Capture" 7 6
      , testCommand "infix with extra arguments" "InfixExtraArg" 8 8
      , testCommand "backtick section applied to its remaining argument" "SectionApp" 6 8
      , testCommand "list use" "ListLiteral" 6 5
      , testCommand "parens around a right-associative body on the LHS" "RightAssocOp" 10 4
      , expectFailBecause "section operators contribute no fixity" $
          testCommand "section operator keeps its fixity" "SectionFixity" 9 5
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
    , testGroup "imports"
      [ testCommand "appends an import" "CrossFileUse" 6 4
      , testCommand "append import at layout column" "IndentImportUse" 6 6
      , testCommand "respells to the target's qualified spelling" "QualifiedScopeUse" 6 4
      , testCommand "import qualified" "QualifiedBodyUse" 5 4
      , testCommand "reject if required import isn't exported" "CrossModuleNotExportedUse" 4 4
      , testCommand "built-in syntax splices without an import" "BuiltinSyntax" 6 4
      , testCommand "record-dot body with the field in scope" "RecordDot" 9 6
      , testCommand "record-dot body with the field imported" "RecordDotImported" 9 4
      , testCommand "refuses a record-dot body when the field is not in scope" "RecordDotUse" 7 4
      , testCommand "refuses a record-dot body without the extension" "RecordDotNoExt" 6 4
      ]
    , testGroup "inline everywhere"
      [ testEverywhere "rewrites all call sites in the module" "Everywhere" 6 4
      , testEverywhere "offered on the top-level binder" "Everywhere" 3 0
      , testEverywhere "offered on a where binder" "Where" 5 4
      , testCase "rewrites the defining module too" $ runWithRetrie $ do
          ddoc <- openDoc "Identity.hs" "haskell"
          adoc <- openDoc "Imported.hs" "haskell"
          _ <- waitForTypecheck adoc
          executeActionAt (T.isSuffixOf " everywhere") (Position 4 6) adoc
          imported <- documentContents adoc
          identity <- documentContents ddoc
          liftIO $ imported @?= T.unlines
            ["module Imported where", "", "import Identity", "", "g x = x"]
          liftIO $ identity @?= T.unlines
            ["module Identity where", "", "-- inline a simple top-level definition", "e :: Int -> Int", "e x = x", "", "f :: Int -> Int", "f x = x"]
      , testCase "rewrites files found via the reference index" $ runWithRetrie $ do
          -- a file referencing the function that the resolve only
          -- discovers through the hiedb reference index; opening it
          -- gets it typechecked and indexed
          doc2 <- openDoc "EverywhereUse2.hs" "haskell"
          _ <- skipManyTill anyMessage $
            referenceReady (\p -> takeFileName p == "EverywhereUse2.hs")
          adoc <- openDoc "EverywhereUse1.hs" "haskell"
          _ <- waitForTypecheck adoc
          executeActionAt (T.isSuffixOf " everywhere") (Position 5 4) adoc
          use1 <- documentContents adoc
          use2 <- documentContents doc2
          liftIO $ use1 @?= T.unlines
            ["module EverywhereUse1 where", "", "import EverywhereDef", "", "f :: Int", "f = 1 + 1"]
          liftIO $ use2 @?= T.unlines
            ["module EverywhereUse2 where", "", "import EverywhereDef", "", "g :: Int", "g = 2 + 1"]
      , testCase "leaves a file whose call site is refused unchanged" $ runWithRetrie $ do
          -- the local where-binding of y captures the y free in the
          -- inlined body, so this file's site is refused; the origin
          -- must still be rewritten
          cdoc <- openDoc "EverywhereCaptureUse.hs" "haskell"
          _ <- skipManyTill anyMessage $
            referenceReady (\p -> takeFileName p == "EverywhereCaptureUse.hs")
          adoc <- openDoc "EverywhereCaptureDef.hs" "haskell"
          _ <- waitForTypecheck adoc
          executeActionAt (T.isSuffixOf " everywhere") (Position 9 4) adoc
          origin <- documentContents adoc
          captured <- documentContents cdoc
          liftIO $ origin @?= T.unlines
            [ "module EverywhereCaptureDef where", ""
            , "y :: Int", "y = 1", ""
            , "e :: Int -> Int", "e x = x + y", ""
            , "f :: Int", "f = 3 + y"
            ]
          liftIO $ captured @?= T.unlines
            [ "module EverywhereCaptureUse where", ""
            , "import EverywhereCaptureDef", ""
            , "r :: Int", "r = e 5", "  where y = 100"
            ]
      ]
    , testGroup "extensions"
      [ testCommand "refuses a QuasiQuotes body into a module without the extension" "QuasiQuoteUse" 5 14
      ]
    , testGroup "cpp"
      [ testCommand "a '#'-led line in a non-CPP module is not a directive" "LabelLine" 15 6
      ]
    , dispatchTest $ testGroup "dispatch"
      [ testCommand "dispatch multi clause" "MultiClause" 7 4
      , testCommand "transfer guards to case" "Guards" 9 4
      , testCommand "multi parameter dispatch" "MultiClauseTuple" 7 6
      , testCommand "multi clause partial" "MultiClausePartial" 8 8
      , testCommand "dispatch wildcard argument" "RecordWildCards2" 14 6
      , testCommand "dispatch wildcard construction from parameters" "RecordWildCardsConstruct" 10 4
      , expectFailBecause "a statically matching constructor argument is not reduced" $
          testCommand "single clause pattern" "Pattern" 5 4
      , expectFailBecause "the bare-reference form is spliced into infix position" $
          testCommand "multi-clause operator used infix" "InfixMultiClause" 7 6
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

-- | Golden test for the single-site inline action at the given position.
testCommand :: TestName -> FilePath -> UInt -> UInt -> TestTree
testCommand title file row col = goldenWithRetrie title file $ \adoc -> do
    _ <- waitForTypecheck adoc
    executeActionAt (not . T.isSuffixOf " everywhere") (Position row col) adoc

-- | Golden test for the inline-everywhere action at the given position.
testEverywhere :: TestName -> FilePath -> UInt -> UInt -> TestTree
testEverywhere title file row col = goldenWithRetrie title file $ \adoc -> do
    _ <- waitForTypecheck adoc
    executeActionAt (T.isSuffixOf " everywhere") (Position row col) adoc

-- | Resolve and execute the sole code action at the position whose title
-- satisfies the predicate.
executeActionAt :: (Text -> Bool) -> Position -> TextDocumentIdentifier -> Session ()
executeActionAt wanted p adoc = do
    codeActions <- getCodeActions adoc $ Range p p
    case [ca | InR ca@CodeAction{_title} <- codeActions, wanted _title] of
        [ca] -> resolveAndExecuteCodeAction ca
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
