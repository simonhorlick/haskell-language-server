module QualifiedFieldDef (setVal) where

import qualified QualifiedFieldRec as R

-- The body updates the record through the *qualified* field name 'R.rval',
-- which parses here because this module imports the record module qualified
-- as R.
setVal :: Int -> R.Rec -> R.Rec
setVal v r = r { R.rval = v }
