module Capture where

e :: Int -> Int
e x = let y = 1 in y + x

f :: Int -> Int
f y = e y
