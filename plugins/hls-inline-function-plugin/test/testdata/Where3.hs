module Where3 where

e x = addOne x
  where
    addOne :: Int -> Int
    addOne x = x + 1

f x = e x
