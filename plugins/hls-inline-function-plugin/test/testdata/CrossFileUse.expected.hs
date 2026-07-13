module CrossFileUse where

import CrossFileDef (e)
import Data.Maybe (fromMaybe)

f :: Int
f = fromMaybe 0 (Just 7)
