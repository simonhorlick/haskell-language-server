module Let where

e :: Int -> Int
e x = let y = 1 in y + x

f :: Int
f = let y = 1 in y + 2
