{-# LANGUAGE PatternSynonyms #-}
module ImportedPatSynDef (e, pattern One) where

pattern One :: Int
pattern One = 1

-- the body names a pattern synonym the target must import
e :: Int -> Int
e x = x + One
