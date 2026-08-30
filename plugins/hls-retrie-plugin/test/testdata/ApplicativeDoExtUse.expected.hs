{-# LANGUAGE ApplicativeDo #-}
module ApplicativeDoExtUse where

import ApplicativeDoDef (e)
import Control.Applicative (ZipList (..))

f :: [Int]
f = getZipList (do
  x <- ZipList [1]
  y <- ZipList [2]
  pure (x + y))
