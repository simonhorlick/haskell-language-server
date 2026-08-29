{-# LANGUAGE OverloadedRecordDot #-}
module RecordDot where

data R = R { name :: String }

label :: R -> String
label r = r.name

f :: R -> String
f x = x.name
