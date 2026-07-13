module QualifiedCtorUse where

-- 'unwrap' is imported unqualified, but its defining module's 'Wrapped'
-- constructor is in scope here only through the qualified import (Q.Wrapped).
import           QualifiedCtorDef (unwrap)
import qualified QualifiedCtorDef as Q
import QualifiedCtorDef (Wrapped(Wrapped))

-- 'unwrap' matches a constructor pattern, so inlining 'unwrap w' (a
-- non-literal argument) dispatches: @case w of Wrapped n -> n + 1@. The
-- 'Wrapped' constructor lands bare in the dispatch pattern but is not in
-- scope unqualified here, so the plugin appends an import supplying it
-- through its parent type ('import QualifiedCtorDef (Wrapped(Wrapped))'),
-- just as it would for a bare reference in the spliced body.
thing :: Q.Wrapped -> Int
thing w = case w of
  (Wrapped n) -> n + 1
