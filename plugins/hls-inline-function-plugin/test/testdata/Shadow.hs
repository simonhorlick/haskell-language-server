module Shadow where

idy :: Int -> Int
idy x = (\y -> y) x

-- In this example we alpha-rename the y inside the lambda even though it's
-- not necessary.
result :: Int -> Int
result y = idy y
