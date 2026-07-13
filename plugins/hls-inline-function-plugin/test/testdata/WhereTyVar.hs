{-# LANGUAGE ScopedTypeVariables #-}

module WhereTyVar where

-- The body mentions no type variable; only the where-clause signature
-- refers to e's forall'd 'a'. Inlining would splice
-- 'let ys :: [a]; ys = [x] in ys' into f, where 'a' is not bound.
e :: forall a. a -> [a]
e x = ys
  where
    ys :: [a]
    ys = [x]

f :: [Int]
f = e (1 :: Int)
