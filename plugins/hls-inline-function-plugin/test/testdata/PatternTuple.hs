module PatternTuple where

e :: (Int, Int) -> Int
e (a, b) = a + b

f :: Int
f = e (1, 2)
