{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot   #-}

module Overloaded where

data Person = Person { name :: String }
data Company = Company { name :: String }

getName r = r.name

greet p = "Hello " ++ getName p
