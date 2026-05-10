module Ide.Plugin.InlineFunction.Resolve
    ( findInlineCandidate
    , InlineCandidate(..)
    ) where

import qualified Data.Map                        as M
import           Data.Maybe                      (listToMaybe)
import qualified Data.Set                        as S
import           Development.IDE.Core.RuleTypes  (HieAstResult (..))
import           Development.IDE.GHC.Compat      (getSourceNodeIds)
import           Development.IDE.GHC.Compat.Core (Name, RealSrcSpan,
                                                  RenamedSource, isTyConName,
                                                  isTyVarName)
import           Development.IDE.Spans.AtPoint   (pointCommand)
import           GHC.Iface.Ext.Types             (ContextInfo (..), HieAST,
                                                  Identifier,
                                                  IdentifierDetails (identInfo))
import qualified GHC.Iface.Ext.Types             as Hie
import           Language.LSP.Protocol.Types     (Position)

-- | A resolved candidate for inlining.
data InlineCandidate = InlineCandidate
    { name :: !Name
    }

-- | Check the AST for what's currently under the cursor. If it's possible to
-- inline it return an 'InlineCandidate'.
findInlineCandidate
    :: HieAstResult
    -> RenamedSource
    -> Position
    -> Maybe InlineCandidate
findInlineCandidate HAR{hieAst} _rn pos = do
  -- Extract the identifiers under the cursor
  let point = concat $ pointCommand hieAst pos extractIdents
  -- Filter out the Left from identifiers (we only want Name, not ModuleName)
  let names = [(n, ctxs, sp) | (Right n, ctxs, sp) <- point
        -- Restrict to valid contexts
        , isInlineSite ctxs
        -- Omit Identifiers that are types
        , not (isTyConName n || isTyVarName n)
        -- TODO: Omit Identifiers that are not functions
       ]
  -- Take the first result
  (name, callSpan, ctxs) <- listToMaybe names
  pure InlineCandidate
      { name = name
      }

-- | Extract identifiers and their spans from the AST.
extractIdents :: HieAST a -> [(Identifier, [ContextInfo], RealSrcSpan)]
extractIdents ast = map toEntry (M.toList (getSourceNodeIds ast))
  where
    toEntry (ident, det) = (ident, S.toList (identInfo det), Hie.nodeSpan ast)

isInlineSite :: [ContextInfo] -> Bool
isInlineSite = any $ \case
    -- Emit the action at the site of a variable usage.
    Use       -> True
    -- TODO: Emit the action at the site of the function definition.
    _         -> False
