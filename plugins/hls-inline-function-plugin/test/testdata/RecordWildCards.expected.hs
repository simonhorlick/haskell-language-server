{-# LANGUAGE RecordWildCards #-}

module RecordWildCards where

data Config = Config { x :: Int }

-- The 'Config{..}' pattern binds 'x' locally, taking the field name from the
-- record. Inlining 'useConfig' at a call site where the argument substituted
-- for 'y' references an outer 'x' would let that 'x' be captured by the
-- wildcard binding.
useConfig :: Config -> Int -> Int
useConfig c y = let Config{..} = c in x + y

result :: Int -> Int
result x = let Config {x = x1} = Config 10 in x1 + x
