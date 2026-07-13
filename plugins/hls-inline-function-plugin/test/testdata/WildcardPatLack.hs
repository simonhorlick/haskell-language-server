module WildcardPatLack where

import WildcardPatHave (addP)
import WildcardPatRec  (P (..))

-- A second use site of 'addP', in a module that does NOT enable
-- RecordWildCards. Inlining all uses of 'addP' (invoked from the
-- extension-having module) dispatches here too, copying the 'P{..}' pattern
-- into a case alternative spliced in this module -- where '{..}' does not
-- parse ("Illegal `..' in record pattern"). The extension guard checks the
-- requesting module (which has the extension), not this target, so the
-- rewrite is applied here and breaks the module. (expectFail until fixed.)
useIt :: P -> Int
useIt p = addP p
