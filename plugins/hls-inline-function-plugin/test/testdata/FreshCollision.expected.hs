module FreshCollision where

y :: Int
y = 1

y1 :: Int
y1 = 50

e :: Int -> Int
e x = x + y

f :: Int -> Int
f x = e x + y + y1
  where y = 100
