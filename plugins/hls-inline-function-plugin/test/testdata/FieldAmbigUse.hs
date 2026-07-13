{-# LANGUAGE DuplicateRecordFields #-}
module FieldAmbigUse where

import FieldAmbigDef (Only (..), X (..), getMsg, getUnique)

-- A second record with the same '_message' field is in scope here.
data Y = Y { _message :: Bool }

-- Inlining 'getMsg x' would splice its body '_message x'. That '_message' is
-- in scope here through the imported X, but Y's '_message' is too, so the
-- spliced bare selector would be ambiguous ("Ambiguous occurrence _message").
-- The plugin refuses to rewrite this file: the selector's written spelling
-- must resolve uniquely at the target, not merely be in scope, and here it
-- resolves to two fields.
useIt :: X -> Int
useIt x = getMsg x

-- 'getUnique' also reads a bare selector, but '_unique' resolves uniquely
-- here, so the guard lets it through and it still inlines.
useUnique :: Only -> Int
useUnique o = getUnique o
