module MultiFileDef where

import Data.Maybe (fromMaybe)

e :: Maybe Int -> Int
e m = fromMaybe 0 m

usesE :: Int
usesE = e (Just 10)
