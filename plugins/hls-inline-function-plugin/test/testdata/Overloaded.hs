{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Overloaded where

data Person = Person { name :: String }
data Company = Company { name :: String }

e r = r.name

f p = "Hello " ++ e p
