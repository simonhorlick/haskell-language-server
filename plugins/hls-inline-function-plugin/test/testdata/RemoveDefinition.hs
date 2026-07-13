module RemoveDefinition (f, g) where

e :: Int -> Int
e x = x + 1

f :: Int
f = e 3

g :: Int
g = e 4
