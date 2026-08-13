module BuiltinSyntax where

single :: a -> [a]
single x = (:) x []

f :: [Int]
f = single 1
