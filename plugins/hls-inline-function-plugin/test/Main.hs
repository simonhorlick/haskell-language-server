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
  , expectFail $ runTest "Doesn't rename shadowed identifier" "Inline e" "Shadow" (Position 8 6)
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
  , runTest "Inlines a fully-applied call written with '$'" "Inline e" "DollarApp" (Position 6 4)
  , runTest "Renames a do-bound name in the body that would capture an argument" "Inline e" "DoBlockCapture" (Position 13 2)
  , runTest "Inlines mutually recursive bindings" "Inline ping" "MutualRecursion" (Position 9 9)
  , runTest "Keeps every argument of an infix call that also supplies extra arguments" "Inline e" "InfixExtraArg" (Position 9 8)
  , runTest "Handles substitution of record dot syntax correctly" "Inline e" "RecordDot" (Position 7 6)
  , runTest "Lambda with a pattern should be inlined correctly" "Inline e" "PatternLambda" (Position 4 4)
  , runTest "Inlines only the call site under the cursor" "Inline e at this use site" "InlineUseSite" (Position 6 7)
  -- The bare 'e' passed to map is an independent partial-application
  -- site even though it sits inside the enclosing 'e (...)' call's span;
  -- only a bare reference that heads a site is subsumed by it.
  , runTest "Inlines the bare reference under the cursor inside another call's argument" "Inline e at this use site" "UseSiteArg" (Position 6 19)
  , runTest "Identifies the function to inline by definition site, not name" "Inline f" "SameName" (Position 9 4)
  -- Inlining an operator that is also used inside a 'proc' rewrites the ordinary
  -- use but leaves the one inside the arrow notation untouched: arrow command
  -- syntax restricts where a spliced expression may stand, so the call-site search
  -- never inlines inside a 'proc' (see 'findCallSites').
  , runTest "Leaves an arrow command-position use untouched" "Inline >:>" "ProcModule" (Position 10 3)
  , runTest "Leaves a use under a visible type application untouched" "Inline e" "TypeApplication" (Position 5 0)
  -- A backtick section is a site of its own: retrie rewrites it to a
  -- lambda eta-expanding the unsupplied parameter.
  , runTest "Inlines a backtick section by eta-expanding the unsupplied parameter" "Inline add" "Section" (Position 6 8)
  -- A section applied to its remaining argument inlines as the section's
  -- lambda left applied -- a beta redex. Collapsing @(\\x -> x + 2) 5@
  -- to @5 + 2@ would need a section-with-extras rewrite in retrie,
  -- mirroring the parenthesized-infix one (see InfixExtraArg).
  , runTest "Inlines a backtick section applied to its remaining argument" "Inline add" "SectionApp" (Position 3 0)
  , runTest "Parenthesizes a body that is an expression type signature" "Inline e" "TySig" (Position 9 10)
  , runTest "Alpha-renames a captured identifier that appears inside an argument" "Inline e" "ArgRename" (Position 12 6)
  , runTest "Appends an import the inlined body needs to the use-site file when inlining across files" "Inline e" "CrossFileUse" (Position 5 4)
  -- The body's references are checked with the spelling the source uses:
  -- a qualified-only import at the use site does not satisfy the body's
  -- bare reference, so the unqualified import is still added.
  , runTest "Adds an import when the needed name is in scope only qualified at the use site" "Inline e at this use site" "QualifiedScopeUse" (Position 6 4)
  -- The spliced body keeps the defining module's qualified spelling
  -- (DM.fromMaybe), so the synthesized import is qualified to match.
  , runTest "Adds a qualified import matching the body's spelling when inlining across files" "Inline e at this use site" "QualifiedBodyUse" (Position 5 4)
  , runTest "Leaves the use site unchanged when the inlined body needs a binding the defining module does not export" "Inline e" "CrossModuleNotExportedUse" (Position 4 4)
  , runTest "Inlines correctly in a file that uses CPP directives" "Inline e" "Cpp" (Position 11 4)
  -- The fixity environment is keyed by bare operator name, and the merge
  -- of the defining and target modules' fixities is right-biased. The
  -- target's qualified use of FixityOther.>< (infixr 8) therefore clobbers
  -- FixityDef.><'s infix 2, so retrie believes the spliced body binds
  -- tighter than && and omits the parentheses the true fixity requires.
  , expectFail $ runTest "Parenthesizes the spliced body using the defining module's operator fixity when a same-named operator is around" "Inline e" "FixityUse" (Position 9 4)
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

-- | The golden tests only inspect the client-side result of an edit; the
-- server applies the same edit list to its own copy of the document, as a
-- sequence of didChange events some clients derive from the array order.
-- These tests catch a divergence between the two: after executing the
-- action the server must still typecheck the document. CrossFileUse
-- combines a call-site edit with an import inserted above it, Let3 puts a
-- capture-rename and the call-site edit on one line -- both once produced
-- an edit order that desynced the server.
serverSyncTests :: TestTree
serverSyncTests = testGroup "server sync" [
    serverSyncTest "with an import edit above the call site" "CrossFileUse" (Position 5 4) "Inline e"
  , serverSyncTest "with a capture rename beside the call site" "Let3" (Position 5 10) "Inline e"
  ]

serverSyncTest :: TestName -> FilePath -> Position -> T.Text -> TestTree
serverSyncTest title file pos actionTitle =
  testCase title $ runInlineSession $ do
    doc     <- openDoc (file ++ ".hs") "haskell"
    _       <- waitForBuildQueue
    actions <- getCodeActions doc (L.Range pos pos)
    action  <- pickAction actionTitle actions
    executeCodeAction action
    tc      <- waitForTypecheck doc
    liftIO $ assertBool "server no longer typechecks the document" (either (const False) id tc)

-- | Regression tests for bugs found by running the inline-function-soak
-- executable over the HLS codebase itself. Each stays 'expectFail' until
-- its bug is fixed; the comments name the finding that produced it.
soakRegressionTests :: TestTree
soakRegressionTests = testGroup "soak regressions" [
    -- Found on exe/Wrapper.hs (launchErrorLSP's defaultArguments): the
    -- inlined binding's body references D.e, and the occurrence name of
    -- that qualified reference matches the binder itself. A qualified
    -- reference keeps its qualifier in the spliced text, so it can
    -- never be captured; the capture check now considers only bare
    -- spellings (retrie's ContextCapture). Before the fix the spurious
    -- rename collided with the call-site substitution, corrupting
    -- every site ("print (D.e (Just 1))1"). Both binder kinds are
    -- pinned: a do-let and a where binding (the where shape is ghcide
    -- PluginUtils's mkFormattingHandlers).
    serverSyncTest "A qualified same-named body reference does not corrupt the splice" "QualifiedSameName" (Position 7 8) "Inline e"
  , serverSyncTest "A qualified same-named where binding does not corrupt the splice" "QualifiedSameNameWhere" (Position 6 10) "Inline e"
    -- Found on ghcide's mkHiFileResult: the body constructs R{..} with
    -- RecordWildCards from the function's own parameters. Substitution
    -- replaces the parameters with the argument expressions, so the
    -- names feeding the wildcard vanish and the construction does not
    -- compile. The candidate is not offered.
  , runActionTest "RecordWildCards construction from parameters offers no Inline action" "RecordWildCardsConstruct" (Position 10 4) []
    -- ...but a wildcard fed by where binders travels with the body
    -- (they become let bindings at the splice), so that shape stays
    -- offerable and must still typecheck after inlining. This guards
    -- the refusal above against over-restriction.
  , serverSyncTest "A wildcard construction fed by where binders still inlines" "RecordWildCardsWhere" (Position 13 4) "Inline mk"
    -- Found on ghcide's mkHomeModLocation (Compat.Core -> FindImports):
    -- retrie printed replacement fragments after stripping the entry
    -- whitespace from the first line only, skewing a multi-line body's
    -- internal layout by the amount stripped. Fixed by printing with a
    -- zeroed entry delta (retrie's Replace.replaceImpl).
  , serverSyncTest "A multi-line let body keeps its bindings aligned when spliced into a do-bind" "MultiLineLetUse" (Position 6 13) "Inline mk"
    -- Found on ghcide's Preprocessor.hs (cppLog): a body whose top-level
    -- operator is '$' spliced as the left operand of a ':' section must
    -- be parenthesized -- a section operand binds tighter than the
    -- section's operator. Fixed by giving section operands the same
    -- precedence context as infix-application operands (retrie's
    -- Context.updExp).
  , serverSyncTest "A '$' body is parenthesized when spliced into a section operand" "SectionOperand" (Position 6 9) "Inline e"
    -- Found on ghcide's reportImportCyclesRule: a same-line where block
    -- ("where pick 0 = 0" with siblings aligned under it) converts to a
    -- let whose entry is one space wide, so the stripped-entry printing
    -- bug above skewed it by one column. Same fix.
  , serverSyncTest "A same-line where block stays aligned when converted to a let" "WhereMulti" (Position 11 4) "Inline mk"
    -- Found on hls-plugin-api's Properties.hs (parseEither): renaming a
    -- binder to avoid capture must rename the binder's type signature
    -- too, or the result has a signature without a binding. The capture
    -- here is genuine (the call site's let-bound k shadows the body's
    -- free k); retrie's occurrence index now includes the names that
    -- signatures mention.
  , serverSyncTest "A capture rename also renames the binder's type signature" "SigRename" (Position 10 9) "Inline e"
    -- Found on ghcide's defDocumentSymbol (Outline.hs): a record
    -- update's head must stay atomic, so a body spliced there needs
    -- parentheses. Fixed by giving the head child atomic precedence
    -- context (retrie's Context.updExp RecordUpd).
  , serverSyncTest "A spliced body keeps its parens under a multi-line record update" "RecordUpdateHead" (Position 9 10) "Inline e"
    -- Found on ghcide's getClientConfigAction (Rules -> Session): the
    -- spliced body needs a bare name that must be imported at the
    -- target, but the target already has a same-named record field
    -- selector in scope from another import, so the added import would
    -- make the bare reference ambiguous. The target is refused: the
    -- import check resolves spellings with the renamer's own lookup,
    -- which sees field selectors.
  , serverSyncTest "An added import does not make a body reference ambiguous" "AmbigUse" (Position 6 4) "Inline e"
    -- Found on ghcide's mkDelta (PositionMapping -> Shake): unlike the
    -- case above, no import is added -- the body's bare reference is
    -- already in scope at the target through the defining module's own
    -- open import, but a second open import provides another name with
    -- the same spelling. The target is refused: a spelling must resolve
    -- uniquely at the target, not merely be in scope.
  , serverSyncTest "A body reference stays unambiguous among two open imports" "AmbigTwoUse" (Position 6 4) "Inline e"
    -- ...whereas a same-spelling *value* in scope is already detected
    -- and the target file is correctly left unchanged. This guards the
    -- fix for the field-selector case above: selectors must join this
    -- behavior, not values join the broken one.
  , testCase "Refuses the target when a same-spelling value is in scope" $
      runInlineSession $ do
        doc      <- openDoc "AmbigValUse.hs" "haskell"
        _        <- waitForBuildQueue
        original <- documentContents doc
        actions  <- getCodeActions doc (L.Range (Position 6 4) (Position 6 4))
        action   <- pickAction "Inline e" actions
        executeCodeAction action
        contents <- documentContents doc
        liftIO $ contents @?= original
  ]

-- | A target file that cannot be rewritten is reported in a warning
-- notification while the rest of the edit still applies. The session
-- advertises resolve support so the resolve request can be sent by hand:
-- the warning is emitted before the resolve response, so it can be read
-- off the message stream afterwards.
reportingTests :: TestTree
reportingTests = testGroup "reporting" [
    testCase "Warns about a file whose call sites cannot be rewritten" $
      runSessionWithTestConfig def
        { testDirLocation      = Left testDataDir
        , testPluginDescriptor = plugin
        , testConfigCaps       = codeActionResolveCaps
          -- lsp-test drops window/showMessage notifications by default
        , testConfigSession    = def { ignoreLogNotifications = False }
        } $ const $ do
          doc     <- openDoc "CrossModuleNotExportedUse.hs" "haskell"
          _       <- waitForBuildQueue
          actions <- getCodeActions doc (L.Range (Position 4 4) (Position 4 4))
          action  <- pickAction "Inline e" actions
          _       <- sendRequest SMethod_CodeActionResolve action
          notif   <- skipManyTill anyMessage (message SMethod_WindowShowMessage)
          liftIO $ do
            notif._params._type_ @?= MessageType_Warning
            assertBool "warning names the file" $
              "CrossModuleNotExportedUse.hs" `T.isInfixOf` notif._params._message
            assertBool "warning explains the failure" $
              "cannot be imported" `T.isInfixOf` notif._params._message
  ]

actionTests :: TestTree
actionTests = testGroup "action" [
    runActionTest "Type signature offers no Inline action" "TopLevel" (Position 2 5) []
  , runActionTest "Variables offer no Inline action" "TopLevel" (Position 3 6) []
  , runActionTest "Offers inlining at definition" "Constant" (Position 3 0) ["Inline e"]
  , runActionTest "Recursive functions cannot be inlined" "Recursive" (Position 6 4) []
  -- Inlining a class method would substitute one implementation at a call
  -- site that dispatches through the class dictionary: here `e (5 :: Int)`
  -- means the Int instance's body, not the default.
  , runActionTest "Class methods with an overriding instance cannot be inlined" "ClassInstance" (Position 10 4) []
  -- ...and even without an instance in sight the method stays uninlinable:
  -- instances may exist in other modules or be added later.
  , runActionTest "Class methods offer no Inline action" "Class" (Position 6 6) []
  , runActionTest "Functions that recurse via their where clause cannot be inlined" "WhereRecursion" (Position 11 4) []
  , runActionTest "Functions consisting of guards cannot be inlined" "Guards" (Position 8 4) []
  , runActionTest "Pattern bindings cannot be inlined" "PatternBind" (Position 5 4) []
  , runActionTest "Bindings with multiple clauses cannot be inlined" "MultiClause" (Position 7 4) []
  , runActionTest "Imported names cannot be inlined" "Imported" (Position 5 4) []
  , runActionTest "Functions with no call sites offer no Inline action" "Uncalled" (Position 3 0) []
  , runActionTest "Offers inlining for a definition imported from a local module" "LocalImport" (Position 4 6) ["Inline e", "Inline e at this use site"]
  , runActionTest "Does not offer inlining when there is a RecordWildCards binding in the arguments" "RecordWildCards2" (Position 15 6) []
  , runActionTest "Does not offer inlining when a forall'd type variable in the body would be captured at the call site" "ImplicitForall" (Position 14 8) []
  , runActionTest "Does not offer inlining when a forall'd type variable is referenced only from the where clause" "WhereTyVar" (Position 14 4) []
  , runActionTest "Prevent inlining when the function contains a pattern bind" "Pattern" (Position 5 4) []
  -- A backtick-section operator sits inside the parenthesized section,
  -- which is a rewriteable site of its own, so both scopes are offered.
  , runActionTest "Offers both inline scopes on a backtick-section operator" "Section" (Position 9 13) ["Inline add", "Inline add at this use site"]
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
    -- the server's copy of the document must survive the edit too
  , serverSyncTests
    -- known-broken cases found by the soak executable, expectFail until fixed
  , soakRegressionTests
    -- files that cannot be rewritten are reported, not silently skipped
  , reportingTests
    -- tests that verify the code action is emitted
  , actionTests
  ]
