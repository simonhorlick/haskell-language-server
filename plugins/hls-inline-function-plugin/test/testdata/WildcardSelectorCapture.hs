{-# LANGUAGE RecordWildCards #-}
module WildcardSelectorCapture where

data Bench = Bench { name :: Int }

-- The body references the top-level field selector 'name'.
total :: Bench -> Int
total b = name b + 1

-- The call site is enclosed by a RecordWildCards pattern that binds 'name'
-- implicitly, shadowing the selector, so splicing the body bare would
-- rebind its 'name' reference. The site is refused and the file left
-- unchanged. Detecting this requires the renamed source's pattern-binder
-- index (riPatBinders): the capturing binder is implicit in the '..' and
-- invisible to parser-pass binder collection.
g :: Bench -> Int
g b@Bench{..} = name + total b
