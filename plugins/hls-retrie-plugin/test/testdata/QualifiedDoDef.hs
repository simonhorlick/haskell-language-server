{-# LANGUAGE QualifiedDo #-}
module QualifiedDoDef (e) where

import qualified QualifiedDoM as M

e :: Int -> Maybe Int
e x = M.do
  y <- Just x
  Just (y + 1)
