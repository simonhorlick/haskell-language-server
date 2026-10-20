{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE PartialTypeSignatures    #-}

module Main (main) where

import           Control.Monad                     (replicateM)
import qualified Data.Map                          as M
import           Data.Maybe                        (isJust)
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import qualified Data.Text.IO                      as T
import qualified Development.IDE.GHC.ExactPrint    as ExactPrint
import qualified Development.IDE.Plugin.CodeAction as Refactor
import           GHC.Clock                         (getMonotonicTime)
import           Ide.Logger
import           Ide.Plugin.Config
import qualified Ide.Plugin.Retrie                 as Retrie
import           System.FilePath
import           Test.Hls
import qualified Test.Hls.FileSystem               as FS
import           Text.Printf                       (printf)

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
    , unfoldTests
    , foldTests
    , benchmarkTests
    ]

-- These tests follow the convention that an expression @e@ is being inlined
-- into a function @f@.
inlineThisTests :: TestTree
inlineThisTests =
  testGroup "Inline this"
    [ testGroup "provider"
      [ testProvider "lhs" "Identity" 7 0 ["Unfold f", "Fold f"]
      , testProvider "identifier" "Identity" 7 6 ["Inline e"]
      , testProvider "imported identifier" "Imported" 4 6 ["Inline e"]
      , testProvider "nested where" "NestedWhere" 3 6 ["Inline e"]
      , testProvider "nested let" "NestedLet" 5 5 ["Inline e"]
      , testProvider "class member" "Class" 6 6 []
      , testProvider "pattern-bound argument" "Class" 6 8 []
      , testProvider "record field" "Field" 8 9 []
      , testProvider "record field control" "Field" 8 6 ["Inline g"]
      , testProvider "where binder" "NestedWhere" 5 4 ["Inline e"]
      , testProvider "let binder" "NestedLet" 4 6 ["Inline e"]
      , testProvider "operator" "Operator" 4 8 ["Inline */"]
      , testProvider "rule" "RuleMaybeNothing" 2 11 ["Apply rule maybe/nothing forward", "Apply rule maybe/nothing backwards"]
      , testProvider "type synonym" "TypeSyn" 2 5 ["Unfold UserId", "Fold UserId"]
      , testProvider "multi clause lhs" "MultiClause" 3 0 []
      , testProvider "guarded lhs" "Guards" 4 0 []
      ]
    , testGroup "resolve"
      [ testInline "top level function" "Identity" 7 6
      , testInline "top level function in another file" "Imported" 4 6
      , testInline "nested where function" "NestedWhere" 3 6
      , testInline "nested where function, one of two uses" "NestedWhereTwice" 3 12
      , testInline "inner of two nested uses" "NestedUse" 3 9
      , testInline "inner of two nested uses, top level" "NestedUseTop" 7 9
      , testInline "outer of two nested uses" "NestedUseOuter" 3 6
      , testInline "where binder, every use" "WhereBinder" 5 4
      , testInline "where binder, nested use" "WhereBinderNested" 5 4
      , testInline "recursive where binder is refused" "WhereBinderRec" 5 4
      , testInline "let binder" "NestedLet" 4 6
      , testInline "nested let function" "NestedLet" 5 5
      , testInline "nested function shadowing a top-level one" "ShadowLocal" 6 26
      , testInline "guard let function" "GuardLet" 5 12
      , testInline "operator" "Operator" 4 8
      , testInline "uses imported fixities" "OpChain" 8 4
      , testInline "definition uses imported fixities" "OpChainDef" 8 4
      , testInline "expression has lower precedence" "Parenthesis" 7 8
      , testInline "partially applied function" "Partial" 7 8
      , testInline "infix with extra arguments" "InfixExtraArg" 8 8
      , testInline "list use" "ListLiteral" 6 5
      , testInline "right-associative expression needs parens" "RightAssocOp" 8 4
      , testInline "where to let" "WhereToLet" 7 6
      , testInline "where to let arg substitution" "WhereToLetSubst" 6 4
      , expectFail $ testInline "qualified" "Qualified" 5 6
      , testInline "single clause pattern" "Pattern" 5 4
      , testInline "rename let binding" "Capture" 7 6
      , testInline "comment in an argument" "CommentArg" 7 6
      , testInline "comment in an argument, CPP module" "CommentArgCpp" 9 6
      ]
    , testGroup "refused"
      [ testRefused "multi clause" "MultiClause" 7 4
      , testRefused "guards" "Guards" 9 4
      , testRefused "multi parameter" "MultiClauseTuple" 7 6
      , testRefused "multi clause partial" "MultiClausePartial" 8 8
      , testRefused "record wildcard construction" "WildcardCon" 10 4
      ]
    , expectFail $ testGroup "dispatch"
      [ testInline "dispatch multi clause" "MultiClause" 7 4
      , testInline "transfer guards to case" "Guards" 9 4
      , testInline "multi parameter dispatch" "MultiClauseTuple" 7 6
      , testInline "multi clause partial" "MultiClausePartial" 8 8
      ]
    , testGroup "imports"
      [ testInline "appends an import" "CrossFileUse" 6 4
      , testInline "appends an import once when other uses remain" "CrossFileUses" 6 4
      , testInline "import unqualified to match template" "QualifiedScopeUse" 6 4
      , testInline "import qualified" "QualifiedBodyUse" 5 4
      , testRefused "different field selector" "FieldClash" 9 4
      , testRefused "local variable clashes with field selector" "VarClash" 10 4
      , testRefused "clashing field selectors" "FieldFieldClash" 9 4
      , testRefused "clashing record label" "ConPatNoDisamb" 9 4
      , testInline "reject if required import isn't exported" "CrossModuleNotExportedUse" 4 4
      , testInline "import a method exported without its class" "ChildExportUse" 5 4
      , testInline "import a method imported without its class" "LoneMethodUse" 5 4
      , testInline "import a constructor via its type" "ConstructorUse" 5 4
      ]
    ]

unfoldTests :: TestTree
unfoldTests =
  testGroup "Unfold"
    [ testUnfold "top level function" "Identity" 4 0
    , testUnfold "cpp branches" "CPP" 5 0
    , testUnfold "comment in the definition" "UnfoldComment" 4 0

      -- The action is run in the defining module, but the changes all happen
      -- in the target module.
    , testCase "rewrites the defining module too" $ runWithRetrie $ do
        defDoc <- openDoc "CrossFileDef.hs" "haskell"
        useDoc <- openDoc "CrossFileUse.hs" "haskell"
        _ <- waitForTypecheck useDoc
        resolveAndExecuteCodeAction =<< unfoldE defDoc
        useContents <- documentContents useDoc
        defContents <- documentContents defDoc
        useExpected <- liftIO $ T.readFile (testDataDir </> "CrossFileUse.expected.hs")
        defExpected <- liftIO $ T.readFile (testDataDir </> "CrossFileDef.hs")
        liftIO $ useContents @?= useExpected
        liftIO $ defContents @?= defExpected
    ]

-- | The "Unfold e" action on the definition in CrossFileDef.hs.
unfoldE :: TextDocumentIdentifier -> Session CodeAction
unfoldE defDoc = do
    let p = Position 5 0
    actions <- getCodeActions defDoc $ Range p p
    liftIO $ case [ca | InR ca@CodeAction{_title = "Unfold e"} <- actions] of
        [ca] -> pure ca
        cas  -> assertFailure $ "One Unfold e action expected, got " <> show (length cas)

foldTests :: TestTree
foldTests =
  testGroup "Fold"
    [ testFold "top level function" "FoldExpression" 2 0
    ]

testProvider :: TestName -> FilePath -> UInt -> UInt -> [Text] -> TestTree
testProvider title file line row expected = testCase title $ runWithRetrie $ do
    adoc <- openDoc (file <.> "hs") "haskell"
    _ <- waitForTypecheck adoc
    let position = Position line row
    codeActions <- getCodeActions adoc $ Range position position
    liftIO $ map codeActionTitle codeActions @?= map Just expected

testFold :: TestName -> FilePath -> UInt -> UInt -> TestTree
testFold = testResolve $ titleStartsWith "Fold"

testUnfold :: TestName -> FilePath -> UInt -> UInt -> TestTree
testUnfold = testResolve $ titleStartsWith "Unfold"

testInline :: TestName -> FilePath -> UInt -> UInt -> TestTree
testInline = testResolve $ titleStartsWith "Inline"

-- | The Inline action at the position resolves to a refusal: the document
-- is left as it was.
testRefused :: TestName -> FilePath -> UInt -> UInt -> TestTree
testRefused title file row col = testCase title $ runWithRetrie $ do
    adoc <- openDoc (file <.> "hs") "haskell"
    _ <- waitForTypecheck adoc
    before <- documentContents adoc
    let p = Position row col
    codeActions <- getCodeActions adoc $ Range p p
    case filter (titleStartsWith "Inline") codeActions of
        [InR ca] -> resolveAndExecuteCodeAction ca
        cas -> liftIO . assertFailure $
          "One code action expected, got " <> show (map codeActionTitle cas)
    after <- documentContents adoc
    liftIO $ after @?= before

testResolve :: (Command |? CodeAction -> Bool) -> TestName -> FilePath -> UInt -> UInt -> TestTree
testResolve keep title file row col = goldenWithRetrie title file $ \adoc -> do
    _ <- waitForTypecheck adoc
    let p = Position row col
    codeActions <- getCodeActions adoc $ Range p p
    case filter keep codeActions of
        [InR ca] -> resolveAndExecuteCodeAction ca
        cas -> liftIO . assertFailure $
          "One code action expected, got " <> show (map codeActionTitle cas)

titleStartsWith :: Text -> Command |? CodeAction -> Bool
titleStartsWith prefix = maybe False (prefix `T.isPrefixOf`) . codeActionTitle

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

-------------------------------------------------------------------------------
-- Benchmark

-- | Timings for the two halves of the inline action on a long module: the
-- code action request (the provider) and resolving the chosen action. The
-- use being inlined sits in the last function of the module, so the walk
-- from the top of the module to the cursor is as long as it gets.
--
-- The timings include the LSP round trip, so they are for comparing one
-- change against another rather than absolute figures. They are reported
-- next to the test name; run just these with @-p Benchmark@.
benchmarkTests :: TestTree
benchmarkTests =
  testGroup "Benchmark"
    [ testCaseInfo "provider" $ runBench $ \doc -> do
        timings <- measure $ do
          actions <- getCodeActions doc (Range benchPosition benchPosition)
          liftIO $ map codeActionTitle actions @?= [Just "Inline e"]
        pure $ report "code action request" timings
    , testCaseInfo "resolve" $ runBench $ \doc -> do
        actions <- getCodeActions doc (Range benchPosition benchPosition)
        ca <- liftIO $ case [ca | InR ca@CodeAction{_title = "Inline e"} <- actions] of
          [ca] -> pure ca
          cas  -> assertFailure $ "One Inline e action expected, got " <> show (length cas)
        -- resolve only: the document is left untouched, so every run does
        -- the same work
        timings <- measure $ do
          CodeAction{_edit} <- resolveCodeAction ca
          liftIO $ assertBool "resolved action carries an edit" (isJust _edit)
        pure $ report "code action resolve" timings
    ]

runBench :: (TextDocumentIdentifier -> Session String) -> IO String
runBench act = runSessionWithTestConfig def
    { testDirLocation = Right benchTree
    , testConfigCaps = codeActionResolveCaps
    , testPluginDescriptor = testPlugins
    } $ \_ -> do
      doc <- openDoc "Bench.hs" "haskell"
      _ <- waitForTypecheck doc
      act doc

benchTree :: FS.VirtualFileTree
benchTree = FS.mkVirtualFileTree testDataDir
    [ FS.directCradle ["Bench.hs"]
    , FS.file "Bench.hs" (FS.sources benchModule)
    ]

-- | Run the action once to warm up, then time 'benchRuns' runs of it.
measure :: Session () -> Session [Double]
measure act = do
    act
    replicateM benchRuns $ do
      start <- liftIO getMonotonicTime
      act
      end <- liftIO getMonotonicTime
      pure (end - start)

benchRuns :: Int
benchRuns = 5

report :: String -> [Double] -> String
report what ts =
    printf "%s: min %.1f ms, mean %.1f ms over %d runs"
      what (1000 * minimum ts) (1000 * sum ts / fromIntegral (length ts)) (length ts)

-- | How many functions the generated module has.
benchFunctions :: Int
benchFunctions = 300

-- | A module of 'benchFunctions' functions, each using @e@ several times.
benchModule :: [Text]
benchModule =
    [ "module Bench where"
    , ""
    , "e :: Int -> Int"
    , "e x = x + 1"
    ] ++ concatMap function [0 .. benchFunctions - 1]
  where
    function :: Int -> [Text]
    function i =
      [ ""
      , "f" <> n <> " :: Int -> Int"
      , "f" <> n <> " x ="
      , "    let y = e x * " <> n
      , "        z = y + e (x + 1)"
      , "    in if y > z then e (y - z) else e (z - y)"
      ]
      where n = T.pack (show i)

-- | The @e@ of @let y = e x@ in the last function of 'benchModule'.
benchPosition :: Position
benchPosition = Position (fromIntegral line) 12
  where
    -- four header lines, six lines per function, the let on the fourth
    line = 4 + 6 * (benchFunctions - 1) + 3
