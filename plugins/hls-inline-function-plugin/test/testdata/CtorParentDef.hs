module CtorParentDef (mk) where

import CtorParentProv (Name (..))

-- The body applies the 'Name' constructor from CtorParentProv.
mk :: Int -> Name
mk n = Name n
