module Guards where

checkValue :: Int -> String
checkValue x
  | x > 0 = "Positive"
  | otherwise = "Negative or Zero"

foo :: String
foo = checkValue 6

