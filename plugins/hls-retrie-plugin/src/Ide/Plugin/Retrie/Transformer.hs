{-# LANGUAGE PatternSynonyms #-}
-- | 'MatchResultTransformer's that can be used with Retrie.
module Ide.Plugin.Retrie.Transformer
  ( overTransformer
  , restrictToSite
  ) where

import           Data.Bifunctor             (second)

import           Development.IDE.GHC.Compat (pattern RealSrcSpan)
import qualified Development.IDE.GHC.Compat as GHC
import qualified GHC                        as GHCGHC

import           Retrie.Types               (Context (..), MatchResult (..),
                                             MatchResultTransformer, Rewrite)
import           Retrie.Universe            (Universe)

-- | Wrap a rewrite's 'MatchResultTransformer'.
overTransformer
  :: (MatchResultTransformer -> MatchResultTransformer)
  -> Rewrite Universe
  -> Rewrite Universe
overTransformer = fmap . second

-- | Refuse every match not containing the given span. Retrie falls
-- through to the next candidate match on 'NoMatch', so only the match
-- at the requested site fires: neither the edits nor the queued
-- imports of any other site reach the emitted 'Change'. The check runs
-- before the wrapped transformer, so refused sites cost it nothing.
restrictToSite :: GHCGHC.RealSrcSpan -> Rewrite Universe -> Rewrite Universe
restrictToSite site = overTransformer $ \orig ctxt match ->
  case ctxtMatchSpan ctxt of
    Just mspan
      | not (RealSrcSpan site Nothing `GHC.isSubspanOf` mspan) ->
          pure NoMatch
    _ -> orig ctxt match
