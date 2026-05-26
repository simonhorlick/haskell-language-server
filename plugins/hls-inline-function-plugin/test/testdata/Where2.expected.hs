module Where2 where

foo x = addOne x
  where
    addOne x = x + 1

result x = let addOne x = x + 1 in addOne x
