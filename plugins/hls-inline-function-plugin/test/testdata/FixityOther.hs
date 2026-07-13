module FixityOther where

infixr 8 ><
(><) :: Int -> Int -> Int
x >< y = x * y
