module MultiLineLetUse where

import MultiLineLetDef (mk)

f :: IO Int
f = do
        r <- mk 5
        pure r
