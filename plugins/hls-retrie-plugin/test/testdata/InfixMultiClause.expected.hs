module InfixMultiClause where

(<+>) :: Int -> Int -> Int
x <+> 0 = x
x <+> y = x + y

f :: Int
f = case (1, 2) of
  (x, 0) -> x
  (x, y) -> x + y
