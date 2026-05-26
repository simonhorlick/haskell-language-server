module InLambda where

addOne :: Int -> Int
addOne x = x + 1

result :: [Int] -> [Int]
result xs = map (\n -> addOne n) xs
