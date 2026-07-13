module SameName where

a :: Int
a = let f = 10 in f

f :: Int -> Int
f x = x + 1

b :: Int
b = f 5
