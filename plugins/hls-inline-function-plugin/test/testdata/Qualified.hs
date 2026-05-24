module Qualified where

foo :: Int -> Int
foo x = x - x

quux y = Qualified.foo y
