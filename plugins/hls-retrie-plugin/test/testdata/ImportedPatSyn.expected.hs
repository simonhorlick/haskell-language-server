{-# LANGUAGE PatternSynonyms #-}
module ImportedPatSyn where

import ImportedPatSynDef (e)
import ImportedPatSynDef (pattern One)

f :: Int
f = 2 + One
