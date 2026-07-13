module RemoveSharedSig (f) where

e, d :: Int -> Int
e x = x + 1
d x = x - 1

f :: Int
f = 3 + 1 + d 4
