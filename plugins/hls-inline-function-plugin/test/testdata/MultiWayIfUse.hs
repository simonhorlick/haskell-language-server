module MultiWayIfUse where

import MultiWayIfDef (grade)

-- Inlining 'grade n' would splice its multi-way 'if' body into this module,
-- which does not enable MultiWayIf, so the result would not parse ("Illegal
-- multi-way if-expression"). The plugin cannot enable the extension the
-- spliced syntax needs, so it offers no action on 'grade'.
useIt :: Int -> String
useIt n = grade n
