module Parenthesis where

e :: Int -> Int -> Int
e x y = x * y

f :: Int
f = e (1 + 2) 3
