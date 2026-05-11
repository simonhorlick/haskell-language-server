module Shadow where

idy :: Int -> Int
idy x = (\y -> y) x

result :: Int -> Int
result y = idy y
