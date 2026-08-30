{-# LANGUAGE TypeApplications #-}
module TypeAppSite where

e :: Num a => a -> a
e x = x + 1

f :: Int
f = e @Int 6
