module TySig where

g :: Int -> Int
g x = x

e :: Int
e = 5 :: Int

operand :: Int
operand = e + 1

argument :: Int
argument = g e
