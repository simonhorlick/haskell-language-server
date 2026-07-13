{-# LANGUAGE RecordWildCards #-}
module WildcardSelectorCapture where

data Bench = Bench { name :: Int }

-- The body references the top-level field selector 'name'.
total :: Bench -> Int
total b = name b + 1

-- The call site is enclosed by a RecordWildCards pattern that binds 'name'
-- implicitly, shadowing the selector, so splicing the body bare would
-- rebind its 'name' reference. The capturing binder has no token of its
-- own -- its occurrence is the '..' itself -- so the capture-avoiding
-- rename must expand the wildcard, pulling the renamed binder out into an
-- explicit field: 'Bench{name = name1, ..}'.
g :: Bench -> Int
g b@Bench{..} = name + total b
