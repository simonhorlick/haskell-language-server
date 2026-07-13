{-# LANGUAGE Arrows #-}
module ProcModule where

import Control.Arrow

-- '>:>' is an ordinary, inlinable operator. It is used both at a normal
-- expression site (in 'h') and at an arrow command-position site inside a
-- 'proc' (in 'f'). Inlining it rewrites the use in 'h' but must leave the
-- command-position use in 'f' untouched, where only a variable may appear.
(>:>) :: ArrowPlus a => a b c -> a b c -> a b c
p >:> q = p <+> q

h :: ArrowPlus a => a Int Int -> a Int Int
h g = g >:> g

f :: ArrowPlus a => a Int Int -> a Int Int
f g = proc x -> (g -< x) >:> (g -< x)
