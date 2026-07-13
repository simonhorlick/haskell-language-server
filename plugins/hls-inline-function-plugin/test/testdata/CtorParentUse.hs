module CtorParentUse where

import CtorParentDef   (mk)
import CtorParentOther (Name)

-- Only CtorParentOther's *type* 'Name' is in scope here, used in 'sig'.
sig :: Name -> Int
sig _ = 0

-- Inlining 'mk 1' would splice its body 'Name 1', which needs
-- CtorParentProv's 'Name' constructor -- not in scope here. The plugin
-- brings a constructor into scope through its parent type ('import
-- CtorParentProv (Name(Name))'), but that import would also bring the
-- *type* 'Name' into scope, turning 'sig's 'Name' ambiguous ("Ambiguous
-- occurrence 'Name'"). The import item is refused because the parent's
-- spelling already means something else here, so the file is left
-- unchanged (with a warning).
useIt n = mk n
