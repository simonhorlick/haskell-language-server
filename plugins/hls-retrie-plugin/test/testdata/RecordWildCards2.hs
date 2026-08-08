{-# LANGUAGE RecordWildCards #-}

module RecordWildCards2 where

data Config = Config { x :: Int }

-- The 'Config{..}' pattern binds 'x' locally, taking the field name from the
-- record. Inlining 'e' at a call site where the argument substituted
-- for 'y' references an outer 'x' would let that 'x' be captured by the
-- wildcard binding.
e :: Config -> Int -> Int
e c@Config{..} y = x + y

f :: Int -> Int
f x = e (Config 10) x
