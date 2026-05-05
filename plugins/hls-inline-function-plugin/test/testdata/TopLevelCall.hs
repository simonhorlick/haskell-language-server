module TopLevelCall where

foo :: Int -> Int
foo x = x + 1

bar :: Int
bar = foo 3
