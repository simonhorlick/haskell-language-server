module ApplicativeDoUse where

import ApplicativeDoDef (e)
import Control.Applicative (ZipList (..))

f :: [Int]
f = getZipList e
