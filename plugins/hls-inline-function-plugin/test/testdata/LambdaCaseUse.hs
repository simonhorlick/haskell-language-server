module LambdaCaseUse where

import LambdaCaseDef (classify, plainShow)

-- Inlining 'classify n' would splice its '\case' body into this module,
-- which does not enable LambdaCase, so the result would not parse ("Illegal
-- \case"). Like the QuasiQuotes and RecordWildCards cases, the plugin
-- cannot enable the extension the spliced syntax needs, so it offers no
-- action on 'classify'. The '\case'-free 'plainShow' from the same module
-- still inlines below.
useIt :: Int -> String
useIt n = classify n

-- 'plainShow' comes from the same LambdaCase module but its body carries no
-- '\case', so it still inlines here.
plain :: Int -> String
plain n = plainShow n
