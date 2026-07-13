{-# LANGUAGE RecordWildCards #-}
module WildcardConstructCapture where

data Rec = Rec { doc :: Int }

-- The body references the top-level field selector 'doc'.
f :: Rec -> Int
f r = doc r + 1

-- 'g' binds 'doc' and reads it implicitly through a 'Rec{..}'
-- construction. Inlining 'f' would splice a reference to the selector,
-- which g's 'doc' would capture: the call site is refused and the file
-- left unchanged (capture-renaming, which would also have to expand the
-- construction's implicit read, lives on the capture-rename branches).
g :: Int -> (Rec, Int)
g doc = (Rec{..}, f r0)
  where r0 = Rec 5
