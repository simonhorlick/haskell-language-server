module PartialLet where

e :: Int -> Int
e x = x * two
  where
    two = 2

f :: [Int]
f = map (\ x -> let
                    two = 2 in x * two) [1, 2, 3]
