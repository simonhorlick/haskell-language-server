module Let where

addOne :: Int -> Int
addOne x = let y = 1 in y + x

result :: Int
result = addOne 2
