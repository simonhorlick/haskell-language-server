{-# LANGUAGE RecordWildCards #-}

module RecordWildCardsConstruct where

data R = R { a :: Int, b :: Int }

mk :: Int -> Int -> R
mk a b = R {..}

f :: R
f = mk 1 2
