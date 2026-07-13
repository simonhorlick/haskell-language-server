module WildcardExtUse where

import WildcardExtDef

-- This module does not enable RecordWildCards. The wildcard fields of 'mk's
-- body come from its parameters, so inlining the call as a dispatch would
-- keep the 'Rec{..}' construction intact -- but the construction would be
-- spliced here, where '{..}' does not parse ("Illegal `..' in record
-- construction. Perhaps you intended to use the RecordWildCards extension").
-- The plugin appends the import it needs but cannot add the language
-- extension the spliced syntax requires, so it offers no action on 'mk'.
thing :: Int -> Int -> Rec
thing x y = mk x y

-- 'mkPlain' comes from the same RecordWildCards module but its body carries
-- no wildcard, so it still inlines here.
plain :: Int -> Int -> Rec
plain x y = mkPlain x y
