module BuiltinSyntax where

single :: a -> [a]
single x = (:) x []

f :: [Int]
f = (:) 1 []
