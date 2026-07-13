{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedRecordDot      #-}
{-# LANGUAGE OverloadedStrings        #-}

module Main ( main ) where

import           Data.Maybe                              (mapMaybe)
import qualified Data.Text                               as T
import           GHC.Paths                               (libdir)
import qualified Ide.Plugin.InlineFunction               as InlineFunction
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

resolveTests :: TestTree
resolveTests = testGroup "resolve" [
    runTest "Inline top-level definition" "Inline e" "TopLevel" (Position 6 4)
  , runTest "Inline constant" "Inline e" "Constant" (Position 6 4)
  , runTest "Rename variables that would be incorrectly captured after substitution" "Inline e" "Capture" (Position 6 6)
  , runTest "Rename only variables that would be incorrectly captured after substitution" "Inline e" "Capture2" (Position 6 6)
  , runTest "Doesn't rename shadowed identifier" "Inline e" "Shadow" (Position 8 6)
  , runTest "Inlines an infix function correctly" "Inline e" "Infix" (Position 6 7)
  , runTest "Inlines let expression correctly" "Inline e" "Let" (Position 6 4)
  , runTest "Inlines parenthesized expression correctly" "Inline e" "Parenthesis" (Position 6 4)
  , runTest "Inlines point free function correctly" "Inline e" "Pointfree" (Position 11 4)
  , runTest "Parenthesizes the inlined body when the context requires it" "Inline e" "BodyParens" (Position 6 8)
  , runTest "Parenthesizes a function-application argument substituted into the body" "Inline e" "ArgParens" (Position 9 10)
  , runTest "Inlines callsites in do-blocks correctly" "Inline e" "DoBlock" (Position 5 11)
  -- Currently duplicates the argument expression at each occurrence. In order
  -- to retain exactly the same behaviour this could be pulled out into a let
  -- binding.
  , runTest "Duplicates the argument when a parameter is used multiple times" "Inline e" "DuplicateArg" (Position 8 4)
  , runTest "Offers inlining for type class methods" "Inline e" "Class" (Position 6 6)
  , runTest "Offers inlining for let bindings" "Inline e" "Let2" (Position 5 5)
  , runTest "Handle capture for let bindings" "Inline e" "Let3" (Position 5 10)
  , runTest "Offers inlining for bindings in where clauses" "Inline e" "Where" (Position 2 6)
  , runTest "Offers inlining for operators" "Inline */" "Operator" (Position 4 8)
  , runTest "Offers inlining for qualified names" "Inline e" "Qualified" (Position 5 6)
  , runTest "Inlines a function that uses overloaded record fields" "Inline e" "Overloaded" (Position 10 18)
  , runTest "Issues substitutions where a free variable of e is captured by a binder outside of e" "Inline e" "CaptureWhere" (Position 9 6)
  , runTest "Picks a fresh name that avoids a top-level definition the surrounding code refers to" "Inline e" "FreshCollision" (Position 12 6)
  , runTest "Renames a capturing binder when the captured free variable is a local binding (no qualified form)" "Inline e" "CaptureLocalBinding" (Position 7 25)
  , runTest "Renames a RecordWildCards binding in the body that would otherwise capture an argument" "Inline e" "RecordWildCards" (Position 14 6)
  , runTest "Renames an explicit RecordWildCards binding in the body that would otherwise capture an argument" "Inline e" "RecordWildCardsExplicit" (Position 10 6)
  , runTest "Un-puns an explicit field beside a '..' wildcard, leaving the wildcard alone" "Inline e" "RecordWildCardsPun" (Position 12 6)
  , runTest "Un-puns only the capturing field when two puns are present" "Inline e" "RecordWildCardsTwoPuns" (Position 12 6)
  , runTest "Moves a functions where clauses to a let binding when inlining" "Inline e" "Where2" (Position 6 6)
  , runTest "Correctly handles type annotations of where clauses" "Inline e" "Where3" (Position 7 6)
  , runTest "Correctly handles parenthesis in where to let clauses" "Inline e" "Where4" (Position 10 7)
  , runTest "Correctly substitutes arguments into the where clause" "Inline e" "Where5" (Position 6 4)
  , runTest "Inlines nested calls of the same function" "Inline e" "Nested" (Position 3 0)
  , runTest "Inlines a call site inside a case branch" "Inline e" "CaseBranch" (Position 7 13)
  , runTest "Inlines a call site inside a lambda body" "Inline e" "InLambda" (Position 6 18)
  , runTest "Inlines every call site that appears in a list literal" "Inline e" "ListLiteral" (Position 6 5)
  , runTest "Creates lambda for partially applied function" "Inline e" "Partial" (Position 6 8)
  , runTest "Creates lambda outside the let statement for partially applied function" "Inline e" "PartialLet" (Position 8 8)
  , runTest "Keeps parentheses around a right-associative body spliced onto the LHS" "Inline e" "RightAssocOp" (Position 10 4)
  , runTest "Inlines a fully-applied call written with '$'" "Inline e" "DollarApp" (Position 8 4)
  , runTest "Renames a do-bound name in the body that would capture an argument" "Inline e" "DoBlockCapture" (Position 13 2)
  , runTest "Inlines mutually recursive bindings" "Inline ping" "MutualRecursion" (Position 9 9)
  , runTest "Keeps every argument of an infix call that also supplies extra arguments" "Inline e" "InfixExtraArg" (Position 9 8)
  , runTest "Handles substitution of record dot syntax correctly" "Inline e" "RecordDot" (Position 7 6)
  , runTest "Lambda with a pattern should be inlined correctly" "Inline e" "PatternLambda" (Position 4 4)
  , runTest "Inlines only the call site under the cursor" "Inline e at this use site" "InlineUseSite" (Position 6 7)
  , runTest "Identifies the function to inline by definition site, not name" "Inline f" "SameName" (Position 9 4)
  -- Inlining an operator that is also used inside a 'proc' rewrites the ordinary
  -- use but leaves the one inside the arrow notation untouched: arrow command
  -- syntax restricts where a spliced expression may stand, so the call-site search
  -- never inlines inside a 'proc' (see 'findCallSites').
  , runTest "Leaves an arrow command-position use untouched" "Inline >:>" "ProcModule" (Position 10 3)
  , runTest "Parenthesizes a body that is an expression type signature" "Inline e" "TySig" (Position 9 10)
  , runTest "Alpha-renames a captured identifier that appears inside an argument" "Inline e" "ArgRename" (Position 12 6)
  , runTest "Appends an import the inlined body needs to the use-site file when inlining across files" "Inline e" "CrossFileUse" (Position 5 4)
  , runTest "Leaves the use site unchanged when the inlined body needs a binding the defining module does not export" "Inline e" "CrossModuleNotExportedUse" (Position 4 4)
  , runTest "Inlines correctly in a file that uses CPP directives" "Inline e" "Cpp" (Position 11 4)
  ]

-- | Inlining every use of a function rewrites every project file that uses it,
-- not just the one the action was invoked in. Triggering inline-all from a use
-- in MultiFileUse also rewrites the use beside the definition in MultiFileDef,
-- so the edit spans both files. The body needs @fromMaybe@: the import is added
-- to MultiFileUse (which lacks it) but not to MultiFileDef (which already has
-- it).
multiFileTests :: TestTree
multiFileTests = testGroup "multi-file" [
    testCase "Inline-all rewrites every project file that uses the function" $
      runInlineSession $ do
        defDoc  <- openDoc "MultiFileDef.hs" "haskell"
        useDoc  <- openDoc "MultiFileUse.hs" "haskell"
        _       <- waitForBuildQueue
        actions <- getCodeActions useDoc (L.Range (Position 5 11) (Position 5 11))
        action  <- pickAction "Inline e" actions
        executeCodeAction action
        defContents <- documentContents defDoc
        useContents <- documentContents useDoc
        liftIO $ do
          defContents @?= T.unlines
            [ "module MultiFileDef where"
            , ""
            , "import Data.Maybe (fromMaybe)"
            , ""
            , "e :: Maybe Int -> Int"
            , "e m = fromMaybe 0 m"
            , ""
            , "usesE :: Int"
            , "usesE = fromMaybe 0 (Just 10)"
            ]
          useContents @?= T.unlines
            [ "module MultiFileUse where"
            , ""
            , "import MultiFileDef (e)"
            , "import Data.Maybe (fromMaybe)"
            , ""
            , "usesEToo :: Int"
            , "usesEToo = fromMaybe 0 (Just 20)"
            ]
  ]

actionTests :: TestTree
actionTests = testGroup "action" [
    runActionTest "Type signature offers no Inline action" "TopLevel" (Position 2 5) []
  , runActionTest "Variables offer no Inline action" "TopLevel" (Position 3 6) []
  , runActionTest "Offers inlining at definition" "Constant" (Position 3 0) ["Inline e"]
  , runActionTest "Recursive functions cannot be inlined" "Recursive" (Position 6 4) []
  , runActionTest "Functions consisting of guards cannot be inlined" "Guards" (Position 8 4) []
  , runActionTest "Pattern bindings cannot be inlined" "PatternBind" (Position 5 4) []
  , runActionTest "Bindings with multiple clauses cannot be inlined" "MultiClause" (Position 7 4) []
  , runActionTest "Imported names cannot be inlined" "Imported" (Position 5 4) []
  , runActionTest "Functions with no call sites offer no Inline action" "Uncalled" (Position 3 0) []
  , runActionTest "Offers inlining for a definition imported from a local module" "LocalImport" (Position 4 6) ["Inline e", "Inline e at this use site"]
  , runActionTest "Does not offer inlining when there is a RecordWildCards binding in the arguments" "RecordWildCards2" (Position 15 6) []
  , runActionTest "Does not offer inlining when a forall'd type variable in the body would be captured at the call site" "ImplicitForall" (Position 14 8) []
  , runActionTest "Prevent inlining when the function contains a pattern bind" "Pattern" (Position 5 4) []
  -- At a use site, both inline-all and inline-this-use-site are offered; at the
  -- definition only inline-all is.
  , runActionTest "Offers both inline scopes at a use site" "InlineUseSite" (Position 6 7) ["Inline e", "Inline e at this use site"]
  , runActionTest "Offers only inline-all at the definition" "InlineUseSite" (Position 3 0) ["Inline e"]
  ]

test :: TestTree
test = testGroup "inline-function" [
    -- tests that invoke the action
    resolveTests
    -- inline-all spans every file that uses the function
  , multiFileTests
    -- tests that verify the code action is emitted
  , actionTests
  ]
