module Recursive where

loop :: Int -> Int
loop x = loop x

result :: Int
result = loop 1
