module MultiClauseLit where

e :: Int -> Int
e 0 = 10
e n = n + 1

a :: Int
a = 10

b :: Int
b = case 5 of
  0 -> 10
  n -> n + 1
