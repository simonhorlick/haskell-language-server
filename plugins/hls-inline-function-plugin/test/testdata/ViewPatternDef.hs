{-# LANGUAGE ViewPatterns #-}
module ViewPatternDef (firstOr) where

-- The parameter is a view pattern, which parses only because this module
-- enables ViewPatterns. A view pattern is not in the rewrite-supported
-- subset, so a non-literal argument inlines as a dispatch that copies the
-- pattern into a case alternative.
firstOr :: Int -> [Int] -> Int
firstOr d (reverse -> (x : _)) = x
firstOr d _                    = d
