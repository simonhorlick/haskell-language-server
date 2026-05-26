module Nested where

addOne :: Int -> Int
addOne x = x + 1

result :: Int
result = addOne (addOne 5)
