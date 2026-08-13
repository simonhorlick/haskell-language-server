module QualifiedScopeUse where

import CrossFileDef (e)
import qualified Data.Maybe as M

f :: Int
f = M.fromMaybe 0 (Just 7)
