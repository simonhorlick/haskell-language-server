module Pointfree where

halve :: Int -> Int
halve x = x `div` 2

double :: Int -> Int
double x = 2 * x

e :: Int -> Int
e = double . halve

f = e 1
