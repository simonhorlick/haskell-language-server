module PartialLet where

e :: Int -> Int
e x = x * two
  where
    two = 2

f :: [Int]
f = map e [1, 2, 3]
