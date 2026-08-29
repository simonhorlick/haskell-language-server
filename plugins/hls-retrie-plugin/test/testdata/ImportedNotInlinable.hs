module ImportedNotInlinable where

import ImportedNotInlinableDef

f :: R -> Int
f r = field r

g :: Int -> Int
g n = m n
