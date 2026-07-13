module DollarApp where

e :: Int -> Int
e x = x + x

f :: Int
f = (\ x -> x + x) $ 5
