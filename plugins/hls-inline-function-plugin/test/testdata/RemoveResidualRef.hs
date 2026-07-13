{-# LANGUAGE TypeApplications #-}

module RemoveResidualRef (f, g) where

e :: forall a. a -> a
e x = x

f :: Int
f = e 3

g :: Int -> Int
g = e @Int
