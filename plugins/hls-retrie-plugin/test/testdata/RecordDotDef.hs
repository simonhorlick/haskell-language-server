{-# LANGUAGE OverloadedRecordDot #-}
module RecordDotDef (R (..), mkR, label) where

data R = R { name :: String }

mkR :: String -> R
mkR = R

label :: R -> String
label r = r.name
