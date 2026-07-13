module Partial where

e :: Int -> Int
e x = x * 2

f :: [Int]
f = map e [1, 2, 3]
