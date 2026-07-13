module MultiClausePartial where

e :: Int -> Int
e 0 = 1
e n = n

a :: [Int]
a = map e [1, 2]
