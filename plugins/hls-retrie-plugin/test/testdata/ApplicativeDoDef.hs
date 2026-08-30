{-# LANGUAGE ApplicativeDo #-}
module ApplicativeDoDef (e) where

import Control.Applicative (ZipList (..))

-- ZipList is Applicative but not Monad: the do block only typechecks
-- under ApplicativeDo
e :: ZipList Int
e = do
  x <- ZipList [1]
  y <- ZipList [2]
  pure (x + y)
