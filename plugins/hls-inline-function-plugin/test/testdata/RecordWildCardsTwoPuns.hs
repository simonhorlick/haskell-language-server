{-# LANGUAGE RecordWildCards #-}

module RecordWildCardsTwoPuns where

data Config = Config { a :: Int, x :: Int }

-- Two explicit puns. Only 'x' captures the argument substituted for 'y', so only
-- it is un-punned; the adjacent 'a' pun is left intact.
e :: Config -> Int -> Int
e c y = let Config {a, x} = c in a + x + y

f :: Int -> Int
f x = e (Config 10 20) x
