module Guards where

e :: Int -> String
e x
  | x > 0 = "Positive"
  | otherwise = "Negative or Zero"

f :: String
f = e 6

