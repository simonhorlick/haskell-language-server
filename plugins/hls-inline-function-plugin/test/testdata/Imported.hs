module Imported where

import           Data.Maybe (fromMaybe)

useImported :: Int
useImported = fromMaybe 0 (Just 7)
