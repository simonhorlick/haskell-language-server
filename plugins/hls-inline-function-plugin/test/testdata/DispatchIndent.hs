module DispatchIndent where

mapFor :: (b -> c) -> (a, b) -> [(a, c)]
mapFor f (hs, m) = [(hs, f m)]

class C t where
  go :: t -> [(Int, Int)]

instance C (Int, Int) where
  go hs = mapFor id hs >>= pure
