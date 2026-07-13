{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE OverloadedRecordDot      #-}
{-# LANGUAGE OverloadedStrings        #-}

module Main ( main ) where

import           Control.Monad                           (void)
import           Data.List                               (isSuffixOf)
import           Data.Maybe                              (mapMaybe)
import qualified Data.Text                               as T
import           Development.IDE.Test                    (referenceReady)
import           GHC.Data.FastString                     (fsLit)
import           GHC.Paths                               (libdir)
import           GHC.Types.SrcLoc                        (mkRealSrcLoc,
                                                          mkRealSrcSpan)
import qualified Ide.Plugin.InlineFunction               as InlineFunction
import           Ide.Plugin.InlineFunction.Remove        (deletionEdits)
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
  , runTest "Refuses a let call site whose binding would capture the body" "Inline e" "Let3" (Position 5 10)
  , runTest "Offers inlining for bindings in where clauses" "Inline e" "Where" (Position 2 6)
  , runTest "Offers inlining for operators" "Inline */" "Operator" (Position 4 8)
  , runTest "Offers inlining for qualified names" "Inline e" "Qualified" (Position 5 6)
  , runTest "Inlines a function that uses overloaded record fields" "Inline e" "Overloaded" (Position 10 18)
  , runTest "Refuses a call site where a where-binding would capture a free variable of e" "Inline e" "CaptureWhere" (Position 9 6)
  , runTest "Refuses a capturing call site rather than renaming around a fresh-name collision" "Inline e" "FreshCollision" (Position 12 6)
  , runTest "Refuses a capturing call site when the captured free variable is a local binding (no qualified form)" "Inline e" "CaptureLocalBinding" (Position 7 25)
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
  , runTest "Inlines a single-clause constructor pattern when the argument matches it" "Inline e" "Pattern" (Position 5 4)
  , runTest "Inlines a single-clause tuple pattern when the argument is a tuple literal" "Inline e" "PatternTuple" (Position 6 4)
  -- Multi-clause definitions: a site is only rewritten when the clause
  -- that fires there is decidable from the argument's syntax. In
  -- MultiClauseCon both sites decide (constructor heads differ
  -- definitely); in MultiClauseLit only 'e 0' does -- 'e 5' stays,
  -- because two different overloaded literals may still be equal under
  -- a lawless Num/Eq instance.
  , runTest "Inlines the clause a constructor argument selects" "Inline e" "MultiClauseCon" (Position 7 4)
  , runTest "Inlines an equal-literal site but not a different-literal one" "Inline e" "MultiClauseLit" (Position 7 4)
  -- Only the selected clause must pass the per-clause checks: the base
  -- case of a recursive definition inlines even though the recursive
  -- clause never can.
  , runTest "Inlines the base case of a recursive multi-clause definition" "Inline e" "MultiClauseRecursive" (Position 7 4)
  -- A guarded clause can fall through even when its patterns match, so
  -- it is never selected; a site that decides an earlier unguarded
  -- clause inlines that clause, and one that reaches the guarded clause
  -- splices the whole dispatch instead.
  , runTest "Inlines a clause decided before a guarded one, dispatching the rest" "Inline e" "MultiClauseGuard" (Position 9 4)
  -- Tier 2: when no single clause is decidable from the argument's
  -- syntax, the call is rewritten to a case expression carrying the
  -- definition's whole dispatch -- clauses, guards and where blocks
  -- verbatim -- so the runtime choice is preserved.
  , runTest "Splices the whole dispatch when no clause is decidable" "Inline e" "MultiClause" (Position 7 4)
  , runTest "Splices a guarded definition as a case keeping its guards" "Inline e" "Guards" (Position 8 4)
  , runTest "Scrutinises an argument tuple for a multi-parameter dispatch" "Inline e" "MultiClauseTuple" (Position 7 6)
  -- A bare reference has no arguments to decide with; the dispatch is
  -- wrapped in a lambda binding fresh argument names.
  , runTest "Wraps the dispatch in a lambda for a bare reference" "Inline e" "MultiClausePartial" (Position 7 8)
  -- Substitution would disconnect a record wildcard from parameters
  -- feeding it, but a dispatch keeps the patterns, so the wildcard's
  -- binders travel intact.
  , runTest "Inlines a wildcard construction from parameters as a dispatch" "Inline mk" "RecordWildCardsConstruct" (Position 10 4)
  , runTest "Inlines a wildcard argument binding as a dispatch" "Inline e" "RecordWildCards2" (Position 14 6)
  , runTest "Refuses a capturing call site even when the capture is via the argument" "Inline e" "ArgRename" (Position 12 6)
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
    -- Found while diagnosing DispatchIndent; same root cause through a
    -- verbatim body instead of the synthetic dispatch case. exact-print
    -- resolves a continuation line's DifferentLine delta against its
    -- layout offset, which class/instance bodies never push (unlike
    -- where/let/do/case-of, whose re-anchoring makes their content
    -- self-healing), so a line continuing the graft's own top-level
    -- expression keeps the column it had at the definition. A body like
    -- "a\n  + b" spliced into a method of an instance whose declarations
    -- start in that column puts "+ 2" level with the method, the layout
    -- rule ends the declaration there, and the module no longer parses
    -- ("parse error on input +"). (expectFail until fixed.)
  , runTest "A multi-line body keeps its continuation lines right of an instance method" "Inline combine" "InstanceIndent" (Position 10 9)
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
  , serverSyncTest "with a capture refusal beside the call site" "Let3" (Position 5 10) "Inline e"
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

-- | Inline-all from a use site in one module and assert a /second/ module,
-- rewritten by the same inline-all, still typechecks. The rewrite only
-- reaches the second module through the hiedb reference index, so the test
-- opens that module first and waits for ghcide's indexing notification
-- (like the call-hierarchy plugin's tests) before requesting the action.
crossModuleSyncTest
  :: TestName -> FilePath -> Position -> T.Text -> FilePath -> TestTree
crossModuleSyncTest title fromFile pos actionTitle checkFile =
  testCase title $ runInlineSession $ do
    check <- openDoc (checkFile ++ ".hs") "haskell"
    skipManyTill anyMessage $ void $
      referenceReady ((checkFile ++ ".hs") `isSuffixOf`)
    from    <- openDoc (fromFile ++ ".hs") "haskell"
    _       <- waitForBuildQueue
    actions <- getCodeActions from (L.Range pos pos)
    action  <- pickAction actionTitle actions
    executeCodeAction action
    tc      <- waitForTypecheck check
    liftIO $ assertBool "the other module no longer typechecks" (either (const False) id tc)

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
    -- compile; clause selection refuses the clause and the site inlines
    -- as a dispatch instead (patterns kept, wildcard intact). The
    -- dispatch output must still typecheck server-side.
  , serverSyncTest "RecordWildCards construction from parameters inlines as a dispatch" "RecordWildCardsConstruct" (Position 10 4) "Inline mk"
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
    -- Found on graphql-engine's schema-parsers Directives.hs (the built-in
    -- directive names, e.g. Name._cached = [G.name|cached|]): the inlined
    -- binding's body is a quasi-quote, which parses only because its
    -- defining module enables QuasiQuotes. Splicing the body verbatim into a
    -- use site whose module does not enable QuasiQuotes yields "parse error
    -- on input |]" -- the plugin appends the quasi-quoter's import but cannot
    -- add the language extension the spliced syntax needs, so the target no
    -- longer parses. A body from a QuasiQuotes module is therefore never
    -- inlined: no action is offered on 'greeting' (defined in QuasiQuoteDef,
    -- which enables the extension), even though the use site is elsewhere.
  , runActionTest "A body defined in a QuasiQuotes module offers no Inline action" "QuasiQuoteUse" (Position 5 14) []
    -- Found on ghcide's DependencyInformation.hs (partitionNodeResults): the
    -- inlined body carries a where clause whose helper 'f' is defined by two
    -- equations. At the call site two names -- 'imps' from the enclosing
    -- pattern and 'errs' from the binding the splice lands in -- collide with
    -- the helper's parameters, so retrie capture-renames them ('imps'->'imps1',
    -- 'errs'->'errs1'). The rename widens the line before the splice, shifting
    -- it one column right, but the continuation lines of the multi-line let
    -- (the where-turned-let's second equation) are indented from the original
    -- pre-shift column -- so the two 'f' equations land one column apart and
    -- the layout no longer parses ("parse error on input f").
  , serverSyncTest "A capture rename beside the splice keeps a multi-equation where-let aligned" "WhereMultiEqn" (Position 10 0) "Inline partitionNodeResults"
    -- Found on ghcide's Session.hs (retryOnSqliteBusy): the inlined body is a
    -- 'let ... in ...' expression. When the call site is a statement in a do
    -- block, the body is spliced at the do-statement column, where its 'let'
    -- is parsed as a do let-statement and the following 'in' -- landing at the
    -- same layout column -- gets a statement separator before it, so it no
    -- longer parses ("parse error on input in"). The spliced 'let ... in' must
    -- be indented past the do-statement column (or parenthesized) to stay one
    -- expression.
  , serverSyncTest "A let-in body is inlined correctly into a do-block statement" "DoBlockLetIn" (Position 12 2) "Inline e"
    -- Found on retrie's PatternMap/Bag.hs (mMatch): 'mapFor f (hs, m)' has
    -- a tuple-pattern parameter, so a call passing a non-tuple argument
    -- cannot decide the clause syntactically and inlines as a
    -- tuple-scrutinee dispatch, @case (q0, q1) of (f, (hs, m)) -> ...@.
    -- 'Dispatch.clauseToAlt' fixes the alternative's indent with
    -- 'DifferentLine 1 2', which does not track where the case is grafted:
    -- inside an instance method (whose equation already sits in column 2)
    -- the alternative lands in column 2 too, level with the method's own
    -- declaration, so it parses as a new binding and the module no longer
    -- typechecks ("Parse error in pattern: f"). The correct indent depends
    -- on the splice column, which the rewrite cannot know when it is built.
    -- (expectFail until fixed.)
  , serverSyncTest "A tuple-dispatch case stays indented below an instance method" "DispatchIndent" (Position 9 10) "Inline mapFor"
    -- Found on ghcide-bench's runBenchmarksFun (Experiments.hs): 'runBench
    -- runSess Bench{..}' uses 'name' from its RecordWildCards parameter, and
    -- it is called inside '\b@Bench{name} -> ... runBench run b'. A
    -- non-record argument makes the call inline as a tuple dispatch
    -- '@case (run, b) of (runSess, Bench{..}) -> ... name ...@'. retrie's
    -- capture analysis used to miss that the dispatch's own 'Bench{..}'
    -- binds 'name' (its scope lookup correlated whole binding nodes, and
    -- the synthesized tuple pattern failed the correlation), so it treated
    -- the spliced body's 'name' as free and renamed the outer binder to
    -- avoid capture -- but 'Bench{name}' is a field pun, and the rename
    -- rewrote it to the invalid 'Bench{name1}' (field 'name1' does not
    -- exist). Fixed by resolving binders per pattern node by exact span
    -- (retrie's riPatBinders): the original patterns inside the
    -- synthesized tuple resolve individually, wildcard binders included,
    -- so no capture is detected and no rename is planned.
  , serverSyncTest "A capture rename expands a NamedFieldPuns binder instead of renaming the field" "PunCapture" (Position 20 27) "Inline runBench"
    -- Found on ghcide-bench's searchSymbol use (Experiments.hs): the same
    -- implicit-wildcard-binder blind spot through a construction instead of
    -- a pattern. The inlined body binds 'doc' with its own RecordWildCards
    -- parameter; retrie treated the spliced 'doc' as free and renamed the
    -- outer 'doc' to avoid capture. That outer 'doc' feeds a 'Rec{..}'
    -- construction, which reads its 'doc' field from a variable spelled
    -- 'doc', so the rename left the field unsupplied ("Constructor 'Rec'
    -- does not have the required strict field(s) doc"). Fixed by the same
    -- per-pattern binder resolution as PunCapture above.
  , serverSyncTest "A dispatch-bound wildcard name is not misdetected as a capture" "WildcardConstruct" (Position 25 14) "Inline combine"
    -- Constructed from the PunCapture analysis (not soak-found): a genuine
    -- capture. The inlined body references the top-level field selector
    -- 'name', and the call site binds 'name' as a NamedFieldPuns pun,
    -- shadowing the selector -- so the body must not be spliced bare.
    -- retrie's rename index once recorded only HsVar occurrences (a
    -- selector occurrence is an XExpr HsRecSelRn node in the renamed
    -- AST), so the capture went entirely undetected and the graft
    -- silently rebound 'name b' to the pun binder -- an 'Int' applied to
    -- an argument, a *deferred* type error in the IDE session, invisible
    -- to 'serverSyncTest' (waitForTypecheck reports success); hence a
    -- golden with an unchanged expectation, pinning the refusal.
  , runTest "A captured field-selector reference refuses the call site under a pun binder" "Inline total" "PunSelectorCapture" (Position 16 25)
    -- The same capture through a RecordWildCards binder instead of a pun
    -- (not soak-found): the capturing binder is implicit in the '..', so
    -- detection must see wildcard-introduced binders (riPatBinders via
    -- the renamed source) to refuse the site.
  , runTest "A capture by a RecordWildCards pattern binder refuses the call site" "Inline total" "WildcardSelectorCapture" (Position 16 23)
    -- The same detection where the capturing binder is read implicitly
    -- by a 'Rec{..}' record construction elsewhere in its scope.
  , runTest "A capture read by a wildcard construction refuses the call site" "Inline f" "WildcardConstructCapture" (Position 15 18)
    -- ...and where the implicit occurrence sits inside the call's own
    -- argument ('f Rec{..}'): both the application-form and the
    -- bare-reference rewrite see the capturing binder and refuse.
  , runTest "A capture whose wildcard occurrence is inside the call argument refuses the site" "Inline f" "WildcardArgCapture" (Position 18 8)
    -- Found on ghcide's mkHiFileResult (Compile.hs -> GHC.Util's
    -- fingerprintToBS, and Session.hs's writeTaskQueue): the inlined function
    -- matches its argument with a constructor pattern ('Fingerprint a b',
    -- 'TaskQueue q'), so a non-literal argument inlines as a dispatch
    -- '@case arg of Wrapped n -> ...@' that puts the constructor bare in the
    -- pattern. The defining module has that constructor in scope unqualified,
    -- but the target has it only through a qualified import ('Util.Fingerprint',
    -- 'Q.Wrapped'), so the bare dispatch pattern would not resolve ("Not in
    -- scope: data constructor Wrapped"). Clause patterns now contribute their
    -- references when a site dispatches, so the import check appends an
    -- import supplying the constructor through its parent type -- pinned
    -- exactly by the golden variant.
  , serverSyncTest "A dispatch on a constructor in scope only qualified at the target brings it into scope" "QualifiedCtorUse" (Position 14 10) "Inline unwrap"
  , runTest "A dispatch constructor is imported through its parent type" "Inline unwrap" "QualifiedCtorUse" (Position 14 10)
    -- Found on hls-graph's shakeNewDatabase (Database.hs -> Internal's
    -- newDatabase): the body constructs a record with a RecordWildCards
    -- wildcard ('Database{..}'), which parses only because the defining module
    -- enables the extension. The wildcard fields come from the parameters, so
    -- the plugin inlines the call as a dispatch that keeps the parameter names
    -- bound and the '{..}' intact -- but it splices the construction into the
    -- target, which does not enable RecordWildCards. The plugin appends the
    -- import the body needs but cannot add the language extension the spliced
    -- syntax requires, so the module would no longer typecheck ("Illegal `..'
    -- in record construction"). Same shape as the QuasiQuotes case, through an
    -- extension the target lacks rather than a quasi-quoter -- but where a
    -- QuasiQuotes module is refused wholesale, RecordWildCards is common
    -- enough that only definitions actually carrying a wildcard are refused:
    -- no action is offered on 'mk', while the wildcard-free 'mkPlain' from
    -- the same module still inlines.
  , runActionTest "A RecordWildCards body is not inlined into a module that lacks the extension" "WildcardExtUse" (Position 12 12) []
  , serverSyncTest "A wildcard-free body from a RecordWildCards module still inlines without the extension" "WildcardExtUse" (Position 17 12) "Inline mkPlain"
    -- Found on ghcide's getParsedModuleRule (Rules.hs): the inlined body's
    -- first line is a '--' line comment before the expression. Grafting the
    -- body transferred the call site's entry delta onto the expression node,
    -- but exact-print applies that entry after the node's prior comments, so
    -- the expression was pulled onto the comment line and commented out
    -- ('-- ... have it' + 'define ...' became '-- ... have itdefine ...') and
    -- the module no longer parsed. Fixed by landing the transferred entry on
    -- the first prior comment instead (retrie's addAllAnnsT); the golden
    -- variant pins the layout: the comment takes the call site's spacing and
    -- the body keeps its newline.
  , serverSyncTest "A leading line comment on the body keeps its newline when spliced" "CommentBody" (Position 9 4) "Inline e"
  , runTest "A leading line comment on the body keeps its newline when spliced (layout)" "Inline e" "CommentBody" (Position 9 4)
    -- Found on hls-cabal-plugin's licenseErrorAction/fieldErrorName use
    -- (Cabal.hs): the inlined body reads a record field selector ('_message')
    -- in scope at the target through an imported record -- but the target has
    -- a second record with the same field, so the spliced bare selector would
    -- be ambiguous ("Ambiguous occurrence _message"). The guard used to miss
    -- it: the selector's spelling reached the import check synthesized from
    -- its 'Name', whose 'OccName' sits in a per-record field namespace that
    -- resolves only against that record's fields. The written spelling (a
    -- plain variable, collected from 'HsRecSelRn' by 'refSpellings') resolves
    -- against every field in scope, so the second field now surfaces and the
    -- target is refused like AmbigTwoUse.
  , serverSyncTest "An already-in-scope field selector in the body does not become ambiguous at the target" "FieldAmbigUse" (Position 15 10) "Inline getMsg"
    -- ...while a selector that resolves uniquely at the target passes the
    -- same guard and still inlines: refusal keys on the second candidate,
    -- not on the reference being a field selector. Golden rather than
    -- server-synced so a wrongly refused (hence unchanged, still
    -- typechecking) document cannot pass.
  , runTest "A uniquely-resolving field selector in the body still inlines" "Inline getUnique" "FieldAmbigUse" (Position 20 14)
    -- Found across ghcide's Compat.Env wrappers (hscSetFlags, hscSetHooks,
    -- hscSetUnitEnv, initTempFs): the body updates a record through a
    -- *qualified* field name ('env { Env.hsc_dflags = df }'), and the target
    -- has the field in scope only under a different qualifier (or
    -- unqualified), so the spliced 'Env.hsc_dflags' would not resolve ("Not
    -- in scope: record field 'Env.hsc_dflags'"). Field labels of record
    -- constructions, updates and constructor patterns now reach the import
    -- check with the spelling the source wrote, so the unresolvable
    -- qualified label refuses the target (fields cannot be imported).
  , serverSyncTest "A qualified record-field update is not inlined where the qualifier is absent" "QualifiedFieldUse" (Position 13 10) "Inline setVal"
    -- Found on ghcide's completion 'go' and hls-cabal-plugin's
    -- listFileCompletions: the body is a '\case' lambda, which parses only
    -- under LambdaCase. Splicing it into a target that lacks the extension
    -- would not parse ("Illegal \case"), and like QuasiQuotes/RecordWildCards
    -- the plugin cannot enable the extension -- so no action is offered,
    -- while the '\case'-free 'plainShow' from the same module still inlines.
  , runActionTest "A LambdaCase body is not inlined into a module that lacks the extension" "LambdaCaseUse" (Position 11 10) []
  , serverSyncTest "A LambdaCase-free body from a LambdaCase module still inlines without the extension" "LambdaCaseUse" (Position 16 10) "Inline plainShow"
    -- Found on ghcide's getCompletionPrefixFromRope: the body is a multi-way
    -- 'if', which parses only under MultiWayIf; splicing it into a target
    -- lacking the extension would not parse ("Illegal multi-way
    -- if-expression"), so no action is offered.
  , runActionTest "A MultiWayIf body is not inlined into a module that lacks the extension" "MultiWayIfUse" (Position 9 10) []
    -- Found on hls-cabal-plugin's moduleOutline and change-type-signature's
    -- stripSignature: a clause parameter is a view pattern, which parses only
    -- under ViewPatterns. A non-literal argument would dispatch, copying the
    -- view pattern into a case alternative spliced at the target, which lacks
    -- the extension ("Illegal view pattern") -- so no action is offered.
  , runActionTest "A view-pattern clause is not dispatched into a module that lacks the extension" "ViewPatternUse" (Position 10 11) []
    -- Found on retrie's dfnsToRewrites and typeSynonymsToRewrites
    -- (Rewrites.hs is Haskell2010): the body contains a tuple section, which
    -- parses only under TupleSections; splicing it into a target lacking the
    -- extension would not parse ("Illegal tuple section"), so no action is
    -- offered.
  , runActionTest "A tuple-section body is not inlined into a module that lacks the extension" "TupleSectionUse" (Position 10 10) []
    -- Found on retrie's mkVarPat inlined into Subst.unpunRenamedFields: the
    -- definition's signature discharged a class constraint at a concrete
    -- instance, and splicing the body into a signature-less where binder
    -- makes GHC re-infer that binder's type, floating a non-type-variable
    -- constraint the target's language edition rejects ("Non type-variable
    -- argument in the constraint... Perhaps FlexibleContexts"). A semantic
    -- effect of losing the signature, invisible to the syntactic extension
    -- guard. (expectFail until fixed.)
  , expectFail $ serverSyncTest "An inlined body does not float constraints its signature solved" "SigDischargeUse" (Position 30 11) "Inline render"
    -- Found on hls-test-utils' standardizeQuotes: the body is a "hanging"
    -- 'let' -- the keyword at the end of the equation's first line, the
    -- bindings and a dedented 'in' left of it -- so its column deltas are
    -- negative relative to the keyword. Grafting it at a column left of the
    -- original underflowed those deltas, printing the bindings at column
    -- zero ("parse error"). retrie now re-lays hanging lets canonically
    -- (first binding beside the keyword, 'in' below it), which is valid at
    -- any column; the golden variant pins that layout.
  , serverSyncTest "A multi-line let body keeps its layout when spliced into an equation" "MultiLineLetBody" (Position 15 10) "Inline standardize"
  , runTest "A hanging let body re-lays canonically when spliced (layout)" "Inline standardize" "MultiLineLetBody" (Position 15 10)
    -- Found on ghcide's mergeEnvs (Compile.hs, 'Inline hsc_env''): the plugin
    -- diffs the exact-printed parsed module to build its edits, but that
    -- print is the *preprocessed* text -- CPP directives and dead '#if'
    -- branches are blank lines there. A multi-line splice near a CPP region
    -- produces diff hunks the algorithm aligns with those blanks, and applied
    -- to the real document they interleave body fragments with the dead
    -- branch (silent corruption, or "parse error"). Edits are vetted
    -- against the real document text and a target whose hunks touch a
    -- preprocessor-rewritten line is refused; golden with an unchanged
    -- expectation, since refusing the file is the only safe outcome.
  , runTest "A splice whose edits touch a CPP region leaves the file unchanged" "Inline e" "CppRegionUse" (Position 16 4)
    -- A capturing call site in a CPP file, with occurrences of the
    -- capturing binder inside a non-active branch. Under the old
    -- capture-renaming semantics this was a pinned hole: the rename
    -- patched only active-branch occurrences (rename information covers
    -- only the active branch), silently rebinding the non-active ones.
    -- A capturing site is now refused outright, which is exactly the
    -- unchanged-file expectation.
  , runTest "A capturing call site in a non-active CPP branch leaves the file unchanged" "Inline e at this use site" "CppRenameUse" (Position 22 5)
    -- ...whereas a multi-line splice whose edits stay on clean lines (the
    -- CPP block is elsewhere in the file) applies through the whole-module
    -- reprint and keeps its layout.
  , runTest "A multi-line splice in a CPP file keeps its indentation" "Inline e at this use site" "CppIndentUse" (Position 22 10)
    -- Found on ghcide's FindImports 'notFound' (and ten sibling soak
    -- violations: showPosition, Spans.Common 'go', compute,
    -- callStackToSrcLoc, generalCompls, getNextPragmaInfo): a multi-line
    -- body spliced inside a '\case' alternative kept its continuation
    -- lines at their original columns. ghc-exactprint printed lambda-case
    -- alternatives without establishing a layout context ('\case' opens
    -- one like 'case..of' does), so grafted subtrees resolved their
    -- column deltas against the enclosing (top-level) offset and landed
    -- left of the alternative's layout ("parse error"). Fixed in
    -- ghc-exactprint's HsLam/HsCmdLam; the goldens pin the re-based
    -- layout for a record construction spliced as a record-update head
    -- and a case body spliced into a parenthesized operand.
  , serverSyncTest "A multi-line record construction re-bases its columns at a deeper splice site" "RecordConstructMulti" (Position 23 10) "Inline mkR"
  , serverSyncTest "A multi-line case body re-bases its columns at a deeper splice site" "CaseBodyIndent" (Position 19 17) "Inline s"
  , runTest "A record construction spliced under a lambda-case keeps valid layout (layout)" "Inline mkR" "RecordConstructMulti" (Position 23 10)
  , runTest "A case body spliced under a lambda-case keeps valid layout (layout)" "Inline s" "CaseBodyIndent" (Position 19 17)
    -- Found on hls-cabal-plugin's cabalPositionToLSPPosition (Position): the
    -- spliced body needs a data constructor not in scope at the target, so
    -- the plugin imports it through its parent type ('import M (Name(Name))').
    -- But that import also brings the *type* 'Name' into scope, so a
    -- same-named type already imported would turn existing references
    -- ambiguous ("Ambiguous occurrence 'Name'"). The import item is now
    -- refused when the parent's spelling already means something else at
    -- the target, leaving the file unchanged.
  , serverSyncTest "A parent-type import for a spliced constructor does not make an existing type ambiguous" "CtorParentUse" (Position 17 10) "Inline mk"
    -- Found on hls-plugin-api's configForPlugin and ghcide's showPosition:
    -- the extension guard used to check only the *requesting* module, so an
    -- inline-all invoked from a RecordWildCards module dispatched a 'P{..}'
    -- pattern into a second module that lacks the extension ("Illegal `..'
    -- in record pattern"). 'rewriteTarget' now re-checks each rewritten
    -- module and leaves an unspliceable one unchanged (with a warning). The
    -- rewrite reaches the second module through the hiedb reference index,
    -- so the test waits for that module to be indexed first.
  , crossModuleSyncTest "A RecordWildCards dispatch is not inlined into a second module that lacks the extension" "WildcardPatHave" (Position 15 13) "Inline addP" "WildcardPatLack"
    -- Found on ghcide's findLocalCompletions ('Inline generalCompls'):
    -- the let-bound body is a multi-line comprehension whose
    -- continuation lines hang left of its head -- legal inside
    -- brackets, where GHC suspends layout, but their column deltas are
    -- negative relative to the head that anchors them. Spliced into
    -- the shallower 'in' body they underflowed, landing left of the
    -- enclosing case alternative's layout column and closing its
    -- layout early ("parse error"). retrie now clamps the hanging
    -- lines to the comprehension's own anchor
    -- ('normalizeHangingComprehensions'), which is valid at any graft
    -- column; the golden pins that layout.
  , serverSyncTest "A hanging comprehension from a let still parses when spliced shallower" "CompreLet" (Position 18 11) "Inline generalThings"
  , runTest "A hanging comprehension re-anchors at its head when spliced (layout)" "Inline generalThings" "CompreLet" (Position 18 11)
    -- Found on ghcide's getNextPragmaInfo (Spans.Pragmas): the body is
    -- a multi-way if. Spliced parenthesized into the deeper 'pure $'
    -- argument of a do statement, the guard continuation lines
    -- resolved against the do block's layout offset instead of
    -- re-anchoring at the if's first guard, landed left of it, and
    -- closed the guard layout early ("parse error"). MultiWayIf opens
    -- a GHC layout context for its guards like '\case' does for
    -- alternatives; fixed in ghc-exactprint's HsMultiIf with the same
    -- setLayoutBoth the HsLam LamCase/LamCases fix added. The golden
    -- pins the re-based guard columns.
  , serverSyncTest "A multi-way-if body still parses when spliced at a deeper column" "MultiWayIfDeep" (Position 22 9) "Inline classify"
  , runTest "A multi-way-if body re-bases its guards at a deeper splice site (layout)" "Inline classify" "MultiWayIfDeep" (Position 22 9)
  ]

-- | Inlining every call site of a module-private definition leaves it
-- unused, so inline-all also deletes the definition and its type
-- signature. Removal is refused whenever a reference could survive the
-- rewrite: the definition is exported (modules outside the reference
-- index may use it), a use is not a rewriteable call site (a visible
-- type application here), the signature also covers another name, or
-- only a single use site was inlined. A recursive clause does not block
-- removal: its self-reference is deleted along with the definition.
-- Local (let/where) bindings are never removed; the existing Let/Where
-- goldens pin that.
removalTests :: TestTree
removalTests = testGroup "definition removal" [
    runTest "Inline-all deletes an unexported definition and its signature" "Inline e" "RemoveDefinition" (Position 3 0)
  , runTest "Inline-all deletes an unexported definition without a signature" "Inline e" "RemoveNoSig" (Position 2 0)
  , runTest "A single-site inline keeps the definition" "Inline e at this use site" "RemoveUseSiteOnly" (Position 6 4)
  , runTest "An exported definition is kept" "Inline e" "RemoveExported" (Position 3 0)
  , runTest "A use under a visible type application keeps the definition" "Inline e" "RemoveResidualRef" (Position 5 0)
  , runTest "A signature covering two names keeps the definition" "Inline e" "RemoveSharedSig" (Position 3 0)
  , runTest "A recursive clause inside the deleted definition does not block removal" "Inline e" "RemoveRecursive" (Position 3 0)
  , serverSyncTest "The module still typechecks after the definition is deleted" "RemoveDefinition" (Position 3 0) "Inline e"
  ]

-- | Unit tests for the pure pieces of the removal logic.
pureTests :: TestTree
pureTests = testGroup "pure" [
    testGroup "deletion edits" [
      testCase "deletes whole lines and swallows the following blank" $
        deletionEdits
          (T.unlines ["module M where", "", "e :: Int", "e = 1", "", "f = 2"])
          [lineSpan 3 3, lineSpan 4 4]
          @?= [wholeLines 2 5]
    , testCase "keeps a non-blank following line" $
        deletionEdits
          (T.unlines ["module M where", "", "e :: Int", "e = 1", "f = 2"])
          [lineSpan 3 3, lineSpan 4 4]
          @?= [wholeLines 2 4]
    , testCase "deletes separated spans independently" $
        deletionEdits
          (T.unlines ["module M where", "", "e :: Int", "other = 9", "e = 1", "f = 2"])
          [lineSpan 3 3, lineSpan 5 5]
          @?= [wholeLines 2 3, wholeLines 4 5]
    , testCase "handles a definition at the end of the file" $
        deletionEdits
          "module M where\n\ne = 1"
          [lineSpan 3 3]
          @?= [wholeLines 2 3]
    , testCase "covers a multi-line definition" $
        deletionEdits
          (T.unlines ["module M where", "e 0 = 0", "e n = n", "", "f = 2"])
          [lineSpan 2 3]
          @?= [wholeLines 1 4]
    ]
  ]
  where
    -- a span within the given 1-based lines; columns are irrelevant to
    -- whole-line deletion
    lineSpan l1 l2 =
      mkRealSrcSpan
        (mkRealSrcLoc (fsLit "M.hs") l1 1)
        (mkRealSrcLoc (fsLit "M.hs") l2 5)
    -- a zero-width whole-line deletion between the given 0-based lines
    wholeLines a b =
      L.TextEdit (L.Range (L.Position a 0) (L.Position b 0)) ""

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
  , runActionTest "Pattern bindings cannot be inlined" "PatternBind" (Position 5 4) []
  , runActionTest "Imported names cannot be inlined" "Imported" (Position 5 4) []
  , runActionTest "Functions with no call sites offer no Inline action" "Uncalled" (Position 3 0) []
  , runActionTest "Offers inlining for a definition imported from a local module" "LocalImport" (Position 4 6) ["Inline e", "Inline e at this use site"]
  , runActionTest "Does not offer inlining when a forall'd type variable in the body would be captured at the call site" "ImplicitForall" (Position 14 8) []
  , runActionTest "Does not offer inlining when a forall'd type variable is referenced only from the where clause" "WhereTyVar" (Position 14 4) []
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
    -- inline-all deletes a definition it leaves unused
  , removalTests
    -- unit tests for the pure removal pieces
  , pureTests
    -- the server's copy of the document must survive the edit too
  , serverSyncTests
    -- known-broken cases found by the soak executable, expectFail until fixed
  , soakRegressionTests
    -- files that cannot be rewritten are reported, not silently skipped
  , reportingTests
    -- tests that verify the code action is emitted
  , actionTests
  ]
