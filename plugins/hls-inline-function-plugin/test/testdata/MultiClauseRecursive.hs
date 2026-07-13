module MultiClauseRecursive where

e :: Int -> Int
e 0 = 1
e n = n * e (n - 1)

a :: Int
a = e 0

b :: Int
b = e 5
