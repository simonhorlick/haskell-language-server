module DollarApp where

e :: Int -> Int
e x = x + x

-- TODO: We should treat this as a fully-applied call site, not as a partial
-- use that needs a lambda.
f :: Int
f = (\x -> x + x) $ 5
