module RemoveRecursive (f) where

e :: Int -> Int
e 0 = 0
e n = e (n - 1)

f :: Int
f = e 0
