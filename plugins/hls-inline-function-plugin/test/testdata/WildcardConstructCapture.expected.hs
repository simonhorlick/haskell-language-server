{-# LANGUAGE RecordWildCards #-}
module WildcardConstructCapture where

data Rec = Rec { doc :: Int }

-- The body references the top-level field selector 'doc'.
f :: Rec -> Int
f r = doc r + 1

-- 'g' binds 'doc' and reads it implicitly through a 'Rec{..}'
-- construction. Inlining 'f' splices a reference to the selector, which
-- g's 'doc' would capture, so the binder is renamed throughout g's scope
-- -- and the construction's implicit read of it must become an explicit
-- field, 'Rec{doc = doc1, ..}', since the '..' offers no token to rename.
g :: Int -> (Rec, Int)
g doc1 = (Rec{doc = doc1, ..}, doc r0 + 1)
  where r0 = Rec 5
