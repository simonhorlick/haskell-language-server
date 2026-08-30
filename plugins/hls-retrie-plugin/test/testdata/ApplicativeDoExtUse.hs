{-# LANGUAGE ApplicativeDo #-}
module ApplicativeDoExtUse where

import ApplicativeDoDef (e)
import Control.Applicative (ZipList (..))

f :: [Int]
f = getZipList e
