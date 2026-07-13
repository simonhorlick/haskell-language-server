{-# LANGUAGE Haskell2010 #-}
module TupleSectionUse where

import TupleSectionDef (tag)

-- Inlining 'tag n' would splice its tuple-section body into this module,
-- which pins Haskell2010 and so lacks TupleSections; the result would not
-- parse ("Illegal tuple section"). The plugin cannot enable the extension
-- the spliced syntax needs, so it offers no action on 'tag'.
useIt :: Int -> [(Int, Bool)]
useIt n = tag n
