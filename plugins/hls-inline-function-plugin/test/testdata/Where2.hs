module Where2 where

e x = addOne x
  where
    addOne x = x + 1

f x = e x
