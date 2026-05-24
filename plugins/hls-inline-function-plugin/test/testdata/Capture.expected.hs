module Capture where

addOne :: Int -> Int
addOne x = let y = 1 in y + x

result :: Int -> Int
result y = let y1 = 1 in y1 + y
