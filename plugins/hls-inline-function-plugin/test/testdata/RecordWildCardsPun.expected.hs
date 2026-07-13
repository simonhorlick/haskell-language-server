{-# LANGUAGE RecordWildCards #-}

module RecordWildCardsPun where

data Config = Config { x :: Int, z :: Int }

-- 'x' is an explicit pun while '..' binds the rest ('z'). Inlining must un-pun
-- only the capturing field, leaving the wildcard alone.
e :: Config -> Int -> Int
e c y = let Config {x, ..} = c in x + y + z

f :: Int -> Int
f x = let Config {x = x1, ..} = Config 10 20 in x1 + x + z
