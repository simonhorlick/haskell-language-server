module MultiClauseGuard where

e :: Bool -> Int
e True = 1
e x
  | x = 2
  | otherwise = 3

a :: Int
a = e True

b :: Int
b = e False
