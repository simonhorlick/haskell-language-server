{-# LANGUAGE RecordWildCards #-}

module RecordWildCardsWhere where

data R = R { a :: Int, b :: Int }

mk :: Int -> R
mk n = R {..}
  where
    a = n
    b = 2

f :: R
f = mk 1
