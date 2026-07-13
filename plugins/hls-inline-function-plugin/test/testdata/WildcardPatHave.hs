{-# LANGUAGE RecordWildCards #-}
module WildcardPatHave (addP, localUse) where

import WildcardPatRec (P (..))

-- 'addP' matches its argument with a RecordWildCards pattern, which parses
-- only because this module enables the extension. The pattern is outside the
-- rewrite-supported subset, so a non-literal argument inlines as a dispatch
-- that copies 'P{..}' into a case alternative.
addP :: P -> Int
addP P{..} = pa + pb

-- A use site inside this (extension-having) module: the extension guard
-- checks this module and lets the inline-all through.
localUse :: P -> Int
localUse p = addP p
