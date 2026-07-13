module Recursive where

e :: Int -> Int
e x = e x

f :: Int
f = e 1
