module InfixExtraArg where

-- A three-parameter function applied in infix form supplies its first two
-- arguments via the operator and the third via ordinary application.
e :: Int -> Int -> Int -> Int
e a b c = a + b + c

f :: Int
f = (1 `e` 2) 3
