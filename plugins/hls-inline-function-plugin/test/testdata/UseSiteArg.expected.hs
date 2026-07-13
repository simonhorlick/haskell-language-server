module UseSiteArg where

e :: Int -> Int
e x = x + 1

f :: [Int] -> Int
f xs = e (sum (map (\x -> x + 1) xs))
