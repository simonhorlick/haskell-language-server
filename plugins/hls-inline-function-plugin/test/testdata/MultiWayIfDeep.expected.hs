{-# LANGUAGE MultiWayIf #-}
module MultiWayIfDeep (classify, useIt) where

-- Inlining 'classify' splices its multi-way-if body, parenthesized,
-- into the deeper 'pure $ ...' argument. The guard continuations used
-- to resolve against the do block's layout offset instead of
-- re-anchoring at the if's first guard, landing left of it and
-- closing the guard layout early ("parse error"). Fixed in
-- ghc-exactprint's HsMultiIf, which now opens a layout context for
-- its guards like case alternatives do. Reduced from ghcide's
-- getNextPragmaInfo (Spans.Pragmas, soak violation).
classify :: Int -> String
classify n =
  if | n > 0
     , n < 10
     -> "small"
     | otherwise
     -> "other"

useIt :: Int -> IO String
useIt n = do
  let extra = 1
  pure $ (if | n + extra > 0
             , n + extra < 10
             -> "small"
             | otherwise
             -> "other")
