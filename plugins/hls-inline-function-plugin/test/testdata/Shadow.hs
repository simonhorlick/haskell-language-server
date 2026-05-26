module Shadow where

idy :: Int -> Int
idy x = (\y -> y) x

-- The lambda's @y@ shadows nothing the splice needs: @x@ sits outside the
-- lambda, so substituting @y@ for @x@ cannot be captured by it.
result :: Int -> Int
result y = idy y
