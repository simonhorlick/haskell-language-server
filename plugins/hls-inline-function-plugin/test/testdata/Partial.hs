module Partial where

double :: Int -> Int
double x = x * 2

bs :: [Int]
bs = map double [1, 2, 3]
