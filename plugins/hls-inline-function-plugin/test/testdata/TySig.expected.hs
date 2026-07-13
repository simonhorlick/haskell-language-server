module TySig where

g :: Int -> Int
g x = x

e :: Int
e = 5 :: Int

operand :: Int
operand = (5 :: Int) + 1

argument :: Int
argument = g (5 :: Int)
