module NotInlinable where

data R = MkR { field :: Int }

f :: R -> Int
f r = field r + x + y
  where
    MkR x = r
    y = 1
