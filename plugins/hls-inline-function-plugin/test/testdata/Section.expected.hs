module Section where

add :: Int -> Int -> Int
add x y = x + y

three :: Int
three = 1 + 2

sums :: [Int]
sums = map (\ x -> x + 2) [5]
