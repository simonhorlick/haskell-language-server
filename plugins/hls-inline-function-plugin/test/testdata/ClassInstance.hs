module ClassInstance where

class Identity x where
    e :: x -> x
    e x = x

instance Identity Int where
    e x = x + 1

f :: Int
f = e (5 :: Int)
