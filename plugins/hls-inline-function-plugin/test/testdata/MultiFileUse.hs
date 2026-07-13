module MultiFileUse where

import MultiFileDef (e)

usesEToo :: Int
usesEToo = e (Just 20)
