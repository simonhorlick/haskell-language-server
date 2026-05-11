module Let where

addOne :: Int -> Int
addOne x = let y = 1 in y + x

result :: Int
result = let y = 1 in y + 2
