module QualifiedSameName where

import qualified CrossFileDef as D

f :: IO ()
f = do
  let e = D.e (Just 1)
  print e
  print e
