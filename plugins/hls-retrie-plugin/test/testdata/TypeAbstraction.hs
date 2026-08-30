{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
module TypeAbstraction where

-- the @a binder is a clause pattern; it must not be copied into a
-- case alternative, so the site is refused
e :: forall a. Show a => a -> String
e @a x = show (x :: a)

f :: String
f = e (1 :: Int)
