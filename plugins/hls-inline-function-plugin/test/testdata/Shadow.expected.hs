module Shadow where

e :: Int -> Int
e x = (\y -> y) x

-- The lambda's @y@ shadows nothing the splice needs: @x@ sits outside the
-- lambda, so substituting @y@ for @x@ cannot be captured by it.
f :: Int -> Int
f y = (\y -> y) y
