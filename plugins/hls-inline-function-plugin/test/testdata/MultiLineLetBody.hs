module MultiLineLetBody where

-- 'standardize's body is a "hanging" 'let': the keyword ends its first line
-- and the bindings and dedented 'in' sit left of it, so their column deltas
-- are negative relative to the keyword and would underflow when spliced at
-- a column left of the original. The splice re-lays the let canonically --
-- first binding beside the keyword, 'in' below it. Found on hls-test-utils'
-- 'standardizeQuotes'.
standardize :: String -> String
standardize msg = let
      repl 'a' = 'x'
      repl c   = c
  in map repl msg

useIt :: String -> String
useIt s = standardize s
