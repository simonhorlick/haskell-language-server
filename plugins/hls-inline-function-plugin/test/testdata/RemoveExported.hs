module RemoveExported (e, f) where

e :: Int -> Int
e x = x + 1

f :: Int
f = e 3
