module QualifiedFieldRec (Rec(..)) where

-- A record whose field 'rval' another module updates through a qualified
-- name.
data Rec = Rec { rval :: Int }
