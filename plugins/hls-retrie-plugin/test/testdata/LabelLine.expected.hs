{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
module LabelLine where

import GHC.OverloadedLabels (IsLabel (..))

instance IsLabel "nine" Int where
  fromLabel = 9

wrap :: Int -> Int
wrap x = x + 1

use :: Int
use = #nine + 1
