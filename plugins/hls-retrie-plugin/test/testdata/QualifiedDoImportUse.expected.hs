{-# LANGUAGE QualifiedDo #-}
module QualifiedDoImportUse where

import QualifiedDoDef (e)
import qualified QualifiedDoM as M

f :: Maybe Int
f = M.do
  y <- Just 1
  Just (y + 1)
