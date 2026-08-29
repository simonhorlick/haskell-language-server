module RecordDotNoExt where

import RecordDotDef (R (..), mkR, label)

-- the extension is off here: the site must be refused
f :: String
f = label (mkR "x")
