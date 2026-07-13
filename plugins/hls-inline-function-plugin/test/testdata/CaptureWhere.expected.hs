module CaptureWhere where

y :: Int
y = 1

e :: Int -> Int
e x = x + y

f :: Int -> Int
f x = e x + y
  where y = 100
