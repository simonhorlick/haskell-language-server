module Section where

add :: Int -> Int -> Int
add x y = x + y

three :: Int
three = add 1 2

sums :: [Int]
sums = map (`add` 2) [5]
