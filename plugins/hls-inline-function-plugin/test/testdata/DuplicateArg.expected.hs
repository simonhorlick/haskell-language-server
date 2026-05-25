module DuplicateArg where

-- 'double' uses its parameter in two places, so inlining
-- duplicates the argument expression at each occurrence.
double :: Int -> Int
double x = x + x

result :: Int
result = 1 + 2 + (1 + 2)
