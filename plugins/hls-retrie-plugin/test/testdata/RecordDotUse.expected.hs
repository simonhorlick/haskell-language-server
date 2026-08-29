{-# LANGUAGE OverloadedRecordDot #-}
module RecordDotUse where

import RecordDotDef (mkR, label)

-- the field is not in scope here: the site must be refused
f :: String
f = label (mkR "x")
