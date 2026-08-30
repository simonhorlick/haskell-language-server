module InfixMultiClause where

(<+>) :: Int -> Int -> Int
x <+> 0 = x
x <+> y = x + y

f :: Int
f = 1 <+> 2
