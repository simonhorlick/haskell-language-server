module Ide.Plugin.InlineFunction.Resolve
    ( findInlineCandidate
    , InlineCandidate(..)
    ) where

import           Development.IDE.Core.RuleTypes  (HieAstResult)
import           Development.IDE.GHC.Compat.Core (Name, RenamedSource)
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
findInlineCandidate har rn pos = Nothing
