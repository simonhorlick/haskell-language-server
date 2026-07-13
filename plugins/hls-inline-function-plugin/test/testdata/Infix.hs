module Infix where

e :: Int -> Int -> Int
e x y = x + y

f :: Int
f = 1 `e` 2
