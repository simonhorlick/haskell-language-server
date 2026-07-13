{-# LANGUAGE TypeApplications #-}

-- | Build the @TextEdit@s that perform the actual source transformation to
-- inline the function.
--
-- This delegates the rewrite entirely to retrie: we construct a retrie
-- rewrite from the function being inlined and hand it to retrie's public
-- 'applyWithRenameInfo', supplying a 'RenameInfo' derived from the renamed
-- source (so capture-avoiding substitution and @RecordWildCards@ binders
-- are handled by retrie). The resulting 'Change' is turned into LSP
-- 'TextEdit's.
module Ide.Plugin.InlineFunction.Rewrite
  ( buildEdits
  , fixityEnvFor
  ) where

import           Control.Exception                     (SomeException)
import           Control.Monad                         (forM)
import           Data.Generics                         (everything, mkQ)
import           Data.List                             (nub)
import           Data.Maybe                            (fromMaybe, mapMaybe)
import           Data.Monoid                           (First (..))
import qualified Data.Set                              as S
import qualified Data.Text                             as T
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.ExactPrint (makeDeltaAst)
import qualified Development.IDE.GHC.Compat.Util       as Util
import           Development.IDE.GHC.Error             (realSrcSpanToRange)
import           Ide.Plugin.InlineFunction.Resolve     (BindingDef (..),
                                                        CallSite (..),
                                                        InlineCandidate (..))
import           Ide.Plugin.InlineFunction.Util        (hsVarName)
import           Language.LSP.Protocol.Types           (TextEdit (..))
import           Retrie                                (Annotated,
                                                        Context (ctxtMatchSpan),
                                                        MatchResult (NoMatch),
                                                        MatchResultTransformer,
                                                        applyWithRenameInfo,
                                                        astA, mkRenameInfo,
                                                        setRewriteTransformer,
                                                        toURewrite, transformA)
import           Retrie.CPP                            (CPP (NoCPP))
import           Retrie.ExactPrint.Annotated           (unsafeMkA)
import           Retrie.Expr                           (mkLocatedHsVar)
import           Retrie.Fixity                         (FixityEnv, mkFixityEnv)
import           Retrie.Monad                          (runRetrie)
import           Retrie.Replace                        (Change (..),
                                                        Replacement (..))
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
  -> IO (Either String [TextEdit])
buildEdits fixities (defSource, defRn) (targetSource, targetRn) candidate = do
  let defAnnotated    = unsafeMkA (makeDeltaAst defSource) 0
      targetAnnotated = unsafeMkA (makeDeltaAst targetSource) 0
      funIdSpan'      = candidate.definition.funIdSpan
      siteSpans       = S.fromList (map application candidate.sites)
      -- retrie needs the RenameInfo to cover every module whose source
      -- contributes to the rewrite: the defining module (the template body
      -- keeps its source spans) and the module being rewritten.
      renameInfo      = mkRenameInfo defRn <> mkRenameInfo targetRn
  result <- try @SomeException $ do
    rewrites <-
      map (setRewriteTransformer (restrictToSites siteSpans))
        <$> constructInlineRewrite defAnnotated funIdSpan'
    if null rewrites
      then pure (rewrites, NoChange)
      else do
        (_, _, change) <-
          runRetrie
            fixities
            (applyWithRenameInfo renameInfo rewrites)
            (NoCPP targetAnnotated)
        pure (rewrites, change)
  pure $ case result of
    Left err          -> Left ("retrie failed: " <> show err)
    Right ([], _)     -> Left "no rewrites produced for the function"
    Right (_, change) -> Right (changeToTextEdits change)

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
-- @funIdSpan@ and construct a retrie rewrite for it.
constructInlineRewrite
  :: Annotated ParsedSource
  -> RealSrcSpan
  -> IO [Rewrite Universe]
constructInlineRewrite annotated funIdSpan =
  fmap astA $ transformA annotated $ \(L _ m) -> do
    let First mfb = everything (<>) (First Nothing `mkQ` matcher) m
    case mfb of
      Just (fun_id, fun_matches) -> do
        fe <- mkLocatedHsVar fun_id
        rewrites <-
          concat
            <$> forM (unLoc (mg_alts fun_matches))
                  (matchToRewrites fe mempty LeftToRight)
        pure (map toURewrite rewrites)
      Nothing -> pure []
  where
    matcher
      :: HsBindLR GhcPs GhcPs
      -> First (LIdP GhcPs, MatchGroup GhcPs (LHsExpr GhcPs))
    matcher FunBind{fun_id, fun_matches}
      | RealSrcSpan sp _ <- getLocA fun_id, sp == funIdSpan =
          First (Just (fun_id, fun_matches))
    matcher _ = First Nothing

-- | Retrie prints a replacement fragment relative to column zero, but a
-- 'TextEdit' splices it verbatim at the replaced span's start column.
-- Shift continuation lines by that column so the fragment keeps its
-- internal layout at the splice point -- a do-block body inserted
-- flush-left would otherwise close the enclosing block's layout context.
indented :: RealSrcSpan -> String -> T.Text
indented sp repl =
  case T.splitOn "\n" (T.pack repl) of
    []           -> ""
    first : rest -> T.intercalate "\n" (first : map indent rest)
  where
    col = srcSpanStartCol sp - 1
    indent l
      | T.null l  = l
      | otherwise = T.replicate col " " <> l

-- | Convert retrie's 'Replacement's to 'TextEdit's. Matching is already
-- restricted to the candidate's call sites (see 'restrictToSites'), so
-- every replacement belongs to the edit: the grafts at the sites plus
-- the capture-avoiding renames their grafts required (whose spans lie
-- elsewhere in the file -- the renamed binder's occurrences).
changeToTextEdits :: Change -> [TextEdit]
changeToTextEdits NoChange = []
changeToTextEdits (Change reps _) =
  -- a rename needed by several sites arrives once per originating site;
  -- collapse the identical copies so the edit has no overlapping ranges
  nub $ mapMaybe toEdit reps
  where
    toEdit Replacement{replLocation, replReplacement}
      | RealSrcSpan sp _ <- replLocation =
          Just (TextEdit (realSrcSpanToRange sp) (indented sp replReplacement))
      | otherwise = Nothing

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
