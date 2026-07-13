module DuplicateArg where

-- 'e' uses its parameter in two places, so inlining
-- duplicates the argument expression at each occurrence.
e :: Int -> Int
e x = x + x

f :: Int
f = 1 + 2 + (1 + 2)
