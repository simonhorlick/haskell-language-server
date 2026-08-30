{-# LANGUAGE ScopedTypeVariables #-}
module ScopedTyVar where

-- the body annotation names the signature's type variable, which is
-- unbound at any call site, so the site is refused
e :: forall a. [a] -> Int
e xs = length (xs :: [a])

f :: Int
f = e [1, 2, 3 :: Int]
