module QualifiedSameNameWhere where

import qualified CrossFileDef as D

f :: Maybe Int -> IO Int
f m = do
    print e
    pure e
  where
    e = D.e m
