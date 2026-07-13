module FixityDef where

infix 2 ><
(><) :: Bool -> Bool -> Bool
x >< y = x && not y

e :: Bool -> Bool
e x = x >< x
