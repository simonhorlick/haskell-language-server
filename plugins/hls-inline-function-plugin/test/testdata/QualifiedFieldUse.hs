module QualifiedFieldUse where

import           QualifiedFieldDef (setVal)
import           QualifiedFieldRec (Rec (..))

-- Inlining 'setVal 0 r' would splice its body 'r { R.rval = 0 }'. The field
-- is spelled 'R.rval', but this module imports the record module
-- unqualified, so 'R' is not a qualifier in scope here and the spliced
-- update would not typecheck ("Not in scope: record field 'R.rval'"). The
-- import check sees the label with the spelling the source wrote and a
-- field cannot be imported, so the plugin refuses to rewrite this file,
-- leaving it unchanged (with a warning).
useIt :: Rec -> Rec
useIt r = setVal 0 r
