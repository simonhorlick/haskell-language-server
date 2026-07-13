module InstanceIndent where

combine :: Int -> Int -> Int
combine a b = a
  + b

class C t where
  go :: t -> Int

instance C Int where
  go n = combine n 2
