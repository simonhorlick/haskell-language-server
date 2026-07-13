module InLambda where

e :: Int -> Int
e x = x + 1

f :: [Int] -> [Int]
f xs = map (\n -> e n) xs
