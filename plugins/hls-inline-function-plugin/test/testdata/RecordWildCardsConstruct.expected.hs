{-# LANGUAGE RecordWildCards #-}

module RecordWildCardsConstruct where

data R = R { a :: Int, b :: Int }

mk :: Int -> Int -> R
mk a b = R {..}

f :: R
f = case (1, 2) of
      (a, b) -> R {..}
