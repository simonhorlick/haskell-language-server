-- | Build the @TextEdit@s that perform the actual source transformation to
-- inline the function.
module Ide.Plugin.InlineFunction.Rewrite
  ( buildEdits
  ) where

import           Control.Monad                         (foldM)
import           Data.Either.Extra                     (maybeToEither)
import           Data.Generics                         (everywhere, mkT)
import qualified Data.Map                              as M
import qualified Data.Text                             as T
import           Development.IDE.GHC.Compat
import           Development.IDE.GHC.Compat.ExactPrint (makeDeltaAst, printA)
import           Ide.Plugin.InlineFunction.Resolve     (BindingDef (..),
                                                        CallSite (..),
                                                        InlineCandidate (..))
import           Ide.Plugin.InlineFunction.Util        (addParens, locateSpan,
                                                        maybeParenthesize,
                                                        replaceAll, replaceExpr)
import           Ide.PluginUtils                       (makeDiffTextEdit)
import           Language.LSP.Protocol.Types           (TextEdit)

-- | Build the text edits that inline @candidate@ in @source@.
--
-- Returns 'Left' if the parsed-source AST does not contain expressions at the
-- spans we expect.
buildEdits
  :: ParsedSource
  -> InlineCandidate
  -> Either String [TextEdit]
buildEdits source candidate = do
  let
    -- annotate the AST with delta locations so that splicing in subtrees
    -- preserves the surrounding layout and comments
    ps = makeDeltaAst source
    bd = candidate.definition
  -- extract the function body
  body <- maybeToEither "inline body not found" (locateSpan bd.bodySpan ps)
  -- inline each callsite in turn
  ps' <- foldM (inlineCallSite bd body) ps candidate.sites
  -- return the differences as TextEdits
  pure $
    makeDiffTextEdit
      (T.pack (printA ps))
      (T.pack (printA ps'))

-- | Replace a single call site with the parameter-substituted function body.
inlineCallSite
  :: BindingDef
  -> LHsExpr GhcPs
  -> ParsedSource
  -> CallSite
  -> Either String ParsedSource
inlineCallSite bd body ps site = do
  -- find ast nodes for each of the arguments based on the spans
  args <-
    traverse
      (\sp ->
        maybeToEither
        ("argument not found at " <> show sp)
        (locateSpan sp ps))
      site.arguments
  let
    substituted = substituteParamsInBody bd args body
    -- parenthesize the body if the surrounding context binds more tightly
    parenthesized = maybeParenthesize site.application ps substituted
  pure $ everywhere (mkT (replaceExpr site.application parenthesized)) ps

-- | Substitute parameters in the body with the arguments at the site of the
-- application.
--
-- One important case to note is that when substituting the arguments we must
-- ensure that the meaning is preserved, i.e. that the argument we're
-- substituting in actually references the same name from the inlining site.
-- As an example:
--   foo x = let y = 1 in x
--   bar y = foo y
-- When we inline the body of foo, the 'y' in the let binding will shadow the
-- argument 'y'. If we naively substitute x ↦ y in the body of foo, the 'y'
-- will incorrectly refer to the let binding. To avoid this we walk the body of
-- foo noting down names that are introduced and their scopes. If an introduced
-- name clashes with an argument, we introduce a let binding at the inlining
-- site and use the original name from the body.
-- In this example,
--   bar y = let x = y in let y = 1 in x
--           ^^^^^^^^^^^^ additional binder to disambiguate
-- The rationale for this is that the user can easily rename the y in the body
-- and run the inline action again to remove the extra let binding.
substituteParamsInBody
  :: BindingDef
  -> [LHsExpr GhcPs]
  -> LHsExpr GhcPs
  -> LHsExpr GhcPs
substituteParamsInBody bd args body =
  let
    -- make a list of replacements for 'param', returns the source span and the
    -- expression that needs to be substituted there
    argSubs (param, arg) =
      let substitution = addParens arg
      in map
        (\span -> (span, substitution))
        (M.findWithDefault [] param bd.paramOccs)
    -- combine replacements for all of the args
    substitutions = M.fromList (concatMap argSubs (zip bd.params args))
  in everywhere (mkT (replaceAll substitutions)) body
