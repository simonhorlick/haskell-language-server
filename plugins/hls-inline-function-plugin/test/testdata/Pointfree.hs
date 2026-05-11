module Pointfree where

halve :: Int -> Int
halve x = x `div` 2

double :: Int -> Int
double x = 2 * x

trip :: Int -> Int
trip = double . halve

site = trip 1
