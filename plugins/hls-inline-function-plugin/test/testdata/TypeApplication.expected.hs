{-# LANGUAGE TypeApplications #-}

module TypeApplication where

e :: forall a. a -> a
e x = x

plain :: Int
plain = 5

-- Instantiating e with a visible type application pins the type in a way
-- the inlined body cannot express, so this use is left untouched.
typed :: Int
typed = e @Int 5
