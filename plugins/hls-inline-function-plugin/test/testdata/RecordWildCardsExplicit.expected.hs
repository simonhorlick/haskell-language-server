{-# LANGUAGE RecordWildCards #-}

module RecordWildCardsExplicit where

data Config = Config { x :: Int }

e :: Config -> Int -> Int
e c y = let Config {x} = c in x + y

f :: Int -> Int
f x = let Config {x = x1} = Config 10 in x1 + x
