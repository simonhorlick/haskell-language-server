module DollarApp where

double :: Int -> Int
double x = x + x

-- TODO: We should treat this as a fully-applied call site, not as a partial
-- use that needs a lambda.
result :: Int
result = double $ 5
