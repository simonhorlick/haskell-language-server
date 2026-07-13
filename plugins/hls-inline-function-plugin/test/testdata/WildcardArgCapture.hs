{-# LANGUAGE RecordWildCards #-}
module WildcardArgCapture where

data Rec = Rec { doc :: Int }

-- The body references the top-level field selector 'doc'.
f :: Rec -> Int
f r = doc r + 1

-- The captured binder's implicit occurrence sits in the call's own
-- argument: 'Rec{..}' reads g's 'doc', and g's 'doc' would capture the
-- body's selector reference. Both rewrite forms see the capturing
-- binder -- the application form refuses the whole call, and the
-- bare-reference form's lambda template carries the same free selector
-- reference, so it refuses the lone 'f' too -- leaving the file
-- unchanged. The lambda fallback with the wildcard expanded lives on
-- the capture-rename branches.
g :: Int -> Int
g doc = f Rec{..}
