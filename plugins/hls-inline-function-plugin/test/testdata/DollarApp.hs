module DollarApp where

e :: Int -> Int
e x = x + x

f :: Int
f = e $ 5
