{-# LANGUAGE TupleSections #-}
module TupleSectionDef (tag) where

-- The body contains a tuple section, which parses only because this module
-- enables TupleSections.
tag :: Int -> [(Int, Bool)]
tag n = map (, True) [n]
