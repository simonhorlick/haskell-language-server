{-# LANGUAGE RecordWildCards #-}
module WildcardExtDef (Rec(..), mk, mkPlain) where

data Rec = Rec { ra :: Int, rb :: Int }

-- The body constructs the record with a RecordWildCards wildcard, which parses
-- only because this module enables the extension.
mk :: Int -> Int -> Rec
mk ra rb = Rec{..}

-- A wildcard-free body from the same module: enabling the extension here must
-- not by itself make the definition unavailable at targets that lack it.
mkPlain :: Int -> Int -> Rec
mkPlain a b = Rec { ra = a, rb = b }
