module WhereRecursion where

-- The self-reference sits in the where clause rather than the body, so
-- the recursion guard must search the whole match to reject e.
e :: Int -> Int
e x = go x
  where
    go 0 = 0
    go n = e (n - 1)

f :: Int
f = e 5
