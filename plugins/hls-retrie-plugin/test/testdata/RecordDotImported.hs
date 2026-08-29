{-# LANGUAGE OverloadedRecordDot #-}
module RecordDotImported where

import RecordDotDef (R (..), mkR, label)

r :: R
r = mkR "x"

f :: String
f = label r
