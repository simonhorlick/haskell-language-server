module MultiClauseLit where

e :: Int -> Int
e 0 = 10
e n = n + 1

a :: Int
a = e 0

b :: Int
b = e 5
