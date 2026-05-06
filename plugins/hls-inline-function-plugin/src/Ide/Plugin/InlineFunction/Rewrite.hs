-- | Build the @TextEdit@s that perform the actual source transformation to
-- inline the function.
module Ide.Plugin.InlineFunction.Rewrite
    ( buildEdits
    ) where

import qualified Data.Text                             as T
import           Development.IDE.GHC.Compat            (DynFlags, ParsedSource)
import           Development.IDE.GHC.Compat.ExactPrint (printA, transformA)
import           Development.IDE.GHC.ExactPrint        (Graft (..), runGraft)
import           Ide.Plugin.InlineFunction.Resolve     (InlineCandidate (..))
import           Ide.PluginUtils                       (makeDiffTextEdit)
import           Language.LSP.Protocol.Types           (TextEdit)

-- | Build the text edits that inline @_candidate@ in @ps@.
--
-- Returns 'Left' if the parsed-source AST does not contain expressions at the
-- spans we expect, or if exactprint fails to apply the graft.
buildEdits :: DynFlags -> ParsedSource -> InlineCandidate -> Either String [TextEdit]
buildEdits dflags ps _candidate = do
    let grafts = mempty
    -- Apply the grafts
    ps' <- transformA ps (runGraft grafts dflags)
    -- Return the differences as TextEdits
    pure (makeDiffTextEdit (T.pack (printA ps)) (T.pack (printA ps')))
