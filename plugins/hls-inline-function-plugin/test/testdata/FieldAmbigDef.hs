{-# LANGUAGE DuplicateRecordFields #-}
module FieldAmbigDef (X(..), getMsg, Only(..), getUnique) where

data X = X { _message :: Int }

-- In this module only X has a '_message' field, so the bare selector resolves.
getMsg :: X -> Int
getMsg x = _message x

data Only = Only { _unique :: Int }

-- '_unique' has no same-named sibling anywhere, so its bare selector use
-- stays unambiguous at any target.
getUnique :: Only -> Int
getUnique o = _unique o
