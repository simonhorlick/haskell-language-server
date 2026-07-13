module SectionOperand where

e :: Int
e = negate $ 1

f :: [[Int]] -> [[Int]]
f = map (e :)
