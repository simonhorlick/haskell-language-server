{-# LANGUAGE TypeApplications #-}

-- | Build the @TextEdit@s that perform the actual source transformation to
-- inline the function.
--
-- This delegates the rewrite entirely to retrie: we construct a retrie
-- rewrite from the function being inlined and hand it to retrie's public
-- 'applyWithRenameInfo', supplying a 'RenameInfo' derived from the renamed
-- source (so capture-avoiding substitution and @RecordWildCards@ binders
-- are handled by retrie). Retrie returns the fully rewritten module -- with
-- its capture-avoiding renames folded into the AST -- which we exact-print
-- and diff against the original print to produce LSP 'TextEdit's, letting
-- exact-print lay out multi-line grafts rather than reassembling them from
-- replacement fragments.
module Ide.Plugin.InlineFunction.Rewrite
  ( buildEdits
  , editsTouchMangledLines
  , fixityEnvFor
  ) where

import           Control.Exception                     (SomeException)
import           Control.Monad                         (forM)
import           Data.Generics                         (everything, mkQ)
import qualified Data.Map                              as M
import           Data.Maybe                            (fromMaybe)
import           Data.Monoid                           (First (..))
import qualified Data.Set                              as S
import qualified Data.Text                             as T
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.ExactPrint (makeDeltaAst)
import qualified Development.IDE.GHC.Compat.Util       as Util
import           Ide.Plugin.InlineFunction.Dispatch    (dispatchRewrites)
import           Ide.Plugin.InlineFunction.Resolve     (BindingDef (..),
                                                        CallSite (..),
                                                        InlineCandidate (..),
                                                        SiteInline (..))
import           Ide.Plugin.InlineFunction.Util        (hsVarName)
import           Ide.PluginUtils                       (makeDiffTextEdit)
import           Language.LSP.Protocol.Types           (Position (..),
                                                        Range (..),
                                                        TextEdit (..))
import           Retrie                                (Annotated,
                                                        Context (ctxtMatchSpan),
                                                        MatchResult (NoMatch),
                                                        MatchResultTransformer,
                                                        applyWithRenameInfo,
                                                        astA, mkRenameInfo,
                                                        setRewriteTransformer,
                                                        toURewrite, transformA)
import           Retrie.CPP                            (CPP (NoCPP), printCPP)
import           Retrie.ExactPrint.Annotated           (printA, unsafeMkA)
import           Retrie.Expr                           (mkLocatedHsVar)
import           Retrie.Fixity                         (FixityEnv, mkFixityEnv)
import           Retrie.Monad                          (changeReplacements,
                                                        runRetrie)
import           Retrie.Replace                        (Replacement (..),
                                                        ReplacementKind (..))
import           Retrie.Rewrites.Function              (matchToRewrites)
import           Retrie.Types                          (Direction (LeftToRight),
                                                        Rewrite)
import           Retrie.Universe                       (Universe)

-- | Build the text edits that inline @candidate@ into the target module.
--
-- The defining module supplies the 'FunBind' the rewrite is constructed
-- from; it may or may not be the module being rewritten.
--
-- The 'FixityEnv' supplies operator precedences so retrie can parenthesize
-- the substituted body correctly; build one via 'fixityEnvFor'.
buildEdits
  :: FixityEnv
  -> (ParsedSource, RenamedSource)
  -- ^ The module defining the function being inlined.
  -> (ParsedSource, RenamedSource)
  -- ^ The module whose call sites are rewritten.
  -> InlineCandidate
  -> IO (Either String (T.Text, [TextEdit], [SrcSpan]))
  -- ^ On success: the exact-printed module the edits' line ranges refer
  -- to; the edits; and the spans of the call sites the rewrite grafted
  -- a body into, which definition-removal decisions key on. The print
  -- is the /preprocessed/ source, so the caller must vet the edits
  -- against the real document text with 'editsTouchMangledLines'
  -- before applying them.
buildEdits fixities (defSource, defRn) (targetSource, targetRn) candidate = do
  let defAnnotated    = unsafeMkA (makeDeltaAst defSource) 0
      targetAnnotated = unsafeMkA (makeDeltaAst targetSource) 0
      funIdSpan'      = candidate.definition.funIdSpan
      -- which spans each clause may rewrite: clause selection assigned
      -- every site the clause that fires there ('selectClauseSites')
      siteSpansByClause =
        M.fromListWith S.union
          [ (i, S.singleton s.application)
          | s <- candidate.sites
          , SiteClause i <- [s.inlineVia]
          ]
      -- the sites where no single clause is decidable; these get the
      -- dispatch-preserving rewrites instead
      dispatchSpans =
        S.fromList
          [s.application | s <- candidate.sites, SiteDispatch <- [s.inlineVia]]
      -- retrie needs the RenameInfo to cover every module whose source
      -- contributes to the rewrite: the defining module (the template body
      -- keeps its source spans) and the module being rewritten.
      renameInfo      = mkRenameInfo defRn <> mkRenameInfo targetRn
  result <- try @SomeException $ do
    (clauseRewrites, dispatchUniverse) <-
      constructInlineRewrite
        defAnnotated
        funIdSpan'
        (M.keysSet siteSpansByClause)
        (not (S.null dispatchSpans))
    -- Each clause's rewrites are restricted to the sites that selected
    -- that clause. A clause whose query also matches another clause's
    -- site (a variable-pattern clause subsumes a literal-pattern one)
    -- refuses it here, and retrie falls through to the next matching
    -- rewrite -- so the clause that fires at runtime is the one spliced.
    -- The dispatch rewrites share one span set: the applied form can
    -- only match a full prefix application and the bare form only a
    -- lone reference, so they never fire at each other's sites.
    let rewrites =
          concat
            [ map (setRewriteTransformer (restrictToSites spans)) rs
            | (i, rs) <- zip [0 ..] clauseRewrites
            , Just spans <- [M.lookup i siteSpansByClause]
            ]
            <> map
                 (setRewriteTransformer (restrictToSites dispatchSpans))
                 dispatchUniverse
    if null rewrites
      then pure (rewrites, (T.empty, [], []))
      else do
        (_, rewritten, change) <-
          runRetrie
            fixities
            (applyWithRenameInfo renameInfo rewrites)
            (NoCPP targetAnnotated)
        -- Diff the whole module printed before and after the rewrite, both
        -- through retrie's exact printer, so only genuine changes surface
        -- as edits and multi-line grafts are laid out by exact-print rather
        -- than reassembled from column-zero replacement fragments. Retrie
        -- folds its capture-avoiding renames into the returned AST (see
        -- 'Retrie.Replace.renameOccurrences'), so the reprint is complete.
        let before = T.pack (printA targetAnnotated)
            after  = T.pack (printCPP [] rewritten)
            grafts =
              [ replLocation r
              | r <- changeReplacements change
              , replKind r == ReplGraft
              ]
        pure (rewrites, (before, makeDiffTextEdit before after, grafts))
  pure $ case result of
    Left err       -> Left ("retrie failed: " <> show err)
    Right ([], _)  -> Left "no rewrites produced for the function"
    Right (_, res) -> Right res

-- | True when any edit's line range touches a line where the
-- exact-printed module and the document text disagree. The parsed
-- module ghcide hands us is the /preprocessed/ source: CPP directives
-- and inactive @#if@ branches are blank lines there, and the
-- diff-derived edits are positioned in that text. An edit confined to
-- lines the two texts share applies to the document verbatim, but one
-- that touches a preprocessor-rewritten line would splice fragments
-- into CPP directives or dead branches, so the file must be left
-- unchanged. Insertions (empty ranges) are vetted against both lines
-- adjacent to the insertion point.
editsTouchMangledLines :: T.Text -> T.Text -> [TextEdit] -> Bool
editsTouchMangledLines printed document = any touches
  where
    printedLines  = M.fromList (zip [0 :: Int ..] (T.lines printed))
    documentLines = M.fromList (zip [0 :: Int ..] (T.lines document))
    differsAt i = M.lookup i printedLines /= M.lookup i documentLines
    touches (TextEdit (Range (Position sl _) (Position el ec)) _) =
      any differsAt [max 0 (start - 1) .. end]
      where
        start = fromIntegral sl
        -- a line-diff range ends at column 0 of the line after the last
        -- one it covers; an insertion's empty range checks its
        -- neighbours instead
        end
          | el > sl, ec == 0 = fromIntegral el - 1
          | otherwise        = fromIntegral el

-- | Refuse any match that does not occur at one of the candidate's call
-- sites. Site policy thereby lives inside the engine, next to match
-- selection: only approved sites are ever rewritten, so every
-- 'Replacement' retrie emits belongs to the edit, and a refused match
-- leaves the traversal free to descend and rewrite an approved site
-- nested within (e.g. the selected call inside a larger application of
-- the same function).
restrictToSites :: S.Set RealSrcSpan -> MatchResultTransformer
restrictToSites sites ctxt match =
  pure $ case ctxtMatchSpan ctxt of
    Just (RealSrcSpan sp _) | sp `S.member` sites -> match
    _                                             -> NoMatch

-- | Find the parsed-source 'FunBind' whose @fun_id@ is located at
-- @funIdSpan@ and construct the retrie rewrites for the clauses in
-- @neededClauses@, in source order (matching the clause indices assigned
-- by clause selection on the renamed source), plus -- when asked for --
-- the dispatch-preserving rewrites for sites where no clause is
-- decidable. Clauses without sites get no rewrites: none would be
-- applied, and a clause outside the rewrite-supported pattern subset
-- (an as-pattern, say) has no retrie query form at all.
constructInlineRewrite
  :: Annotated ParsedSource
  -> RealSrcSpan
  -> S.Set Int
  -> Bool
  -> IO ([[Rewrite Universe]], [Rewrite Universe])
constructInlineRewrite annotated funIdSpan neededClauses needDispatch =
  fmap astA $ transformA annotated $ \(L _ m) -> do
    let First mfb = everything (<>) (First Nothing `mkQ` matcher) m
    case mfb of
      Just (fun_id, fun_matches) -> do
        fe <- mkLocatedHsVar fun_id
        perClause <-
          forM (zip [0 ..] (unLoc (mg_alts fun_matches))) $ \(i, alt) ->
            if i `S.member` neededClauses
              then map toURewrite <$> matchToRewrites fe mempty LeftToRight alt
              else pure []
        dispatch <-
          if needDispatch
            then map toURewrite <$> dispatchRewrites fun_id fun_matches
            else pure []
        pure (perClause, dispatch)
      Nothing -> pure ([], [])
  where
    matcher
      :: HsBindLR GhcPs GhcPs
      -> First (LIdP GhcPs, MatchGroup GhcPs (LHsExpr GhcPs))
    matcher FunBind{fun_id, fun_matches}
      | RealSrcSpan sp _ <- getLocA fun_id, sp == funIdSpan =
          First (Just (fun_id, fun_matches))
    matcher _ = First Nothing

-- | Build the in-scope fixity environment that retrie needs to
-- parenthesize substituted operator expressions correctly.
--
-- Fixities of imported operators live in interface files and can only be
-- fetched per name via 'lookupFixityRn' in the renamer monad, so we collect
-- the operators used in the module and look each one up.
fixityEnvFor :: HscEnv -> TcGblEnv -> RenamedSource -> IO FixityEnv
fixityEnvFor hscEnv tcg rn =
  fmap (mkFixityEnv . fromMaybe [] . snd) $
    initTcWithGbl hscEnv tcg (realSrcLocSpan (mkRealSrcLoc "<dummy>" 1 1)) $
      forM (S.toList (collectOpNames rn)) $ \name -> do
        fixity <-
          Util.handleGhcException
            (const $ pure defaultFixity)
            (lookupFixityRn name)
        let fs = occNameFS (nameOccName name)
        pure (fs, (fs, fixity))

collectOpNames :: RenamedSource -> S.Set Name
collectOpNames = S.fromList . everything (<>) ([] `mkQ` opName)
  where
    opName :: HsExpr GhcRn -> [Name]
    opName (OpApp _ _ (L _ (HsVar _ ident)) _) = [hsVarName ident]
    opName _                                   = []
