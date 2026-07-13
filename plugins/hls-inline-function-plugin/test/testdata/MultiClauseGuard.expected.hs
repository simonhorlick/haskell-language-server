module MultiClauseGuard where

e :: Bool -> Int
e True = 1
e x
  | x = 2
  | otherwise = 3

a :: Int
a = 1

b :: Int
b = case False of
  True -> 1
  x
    | x -> 2
    | otherwise -> 3
