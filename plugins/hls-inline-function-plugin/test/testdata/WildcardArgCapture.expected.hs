{-# LANGUAGE RecordWildCards #-}
module WildcardArgCapture where

data Rec = Rec { doc :: Int }

-- The body references the top-level field selector 'doc'.
f :: Rec -> Int
f r = doc r + 1

-- The captured binder's implicit occurrence sits in the call's own
-- argument: 'Rec{..}' reads g's 'doc'. The application-form rewrite
-- captures the argument into a substitution value whose text is
-- re-printed as part of the graft, where the implicit read has no
-- variable node to rewrite -- so that match must be refused. The
-- bare-reference rewrite then matches just 'f', leaving the argument in
-- place at its own span, where the wildcard expansion repairs it:
-- '(\ r -> doc r + 1) Rec{doc = doc1, ..}'.
g :: Int -> Int
g doc1 = (\ r -> doc r + 1) Rec{doc = doc1, ..}
