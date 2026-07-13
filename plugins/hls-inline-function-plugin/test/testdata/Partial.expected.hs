module Partial where

e :: Int -> Int
e x = x * 2

f :: [Int]
f = map (\x -> x * 2) [1, 2, 3]
