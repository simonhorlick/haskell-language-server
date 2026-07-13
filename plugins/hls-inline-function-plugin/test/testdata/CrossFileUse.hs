module CrossFileUse where

import CrossFileDef (e)

f :: Int
f = e (Just 7)
