module ViewPatternUse where

import ViewPatternDef (firstOr)

-- Inlining 'firstOr 0 xs' would dispatch (the argument is not a literal),
-- copying the clause's view pattern into a case alternative spliced here.
-- This module does not enable ViewPatterns, so the result would not parse
-- ("Illegal view pattern"). The plugin cannot enable the extension the
-- spliced syntax needs, so it offers no action on 'firstOr'.
useIt :: [Int] -> Int
useIt xs = firstOr 0 xs
