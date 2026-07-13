module FixityUse where

import           FixityDef       (e, (><))
import qualified FixityOther     as O

n :: Int
n = 1 O.>< 2

f :: Bool
f = (True >< True) && False
