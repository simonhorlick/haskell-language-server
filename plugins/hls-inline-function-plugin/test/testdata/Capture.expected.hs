module Capture where

addOne :: Int -> Int
addOne x = let y = 1 in y + x

result :: Int -> Int
result y = let y' = 1 in y' + y
