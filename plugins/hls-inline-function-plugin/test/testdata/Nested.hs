module Nested where

e :: Int -> Int
e x = x + 1

f :: Int
f = e (e 5)
