module InfixExtraArg where

-- A three-parameter function applied in infix form supplies its first two
-- arguments via the operator and the third via ordinary application. Inlining
-- must keep all three (see the InfixHead case in 'Ide.Plugin.InlineFunction.CallForm').
e :: Int -> Int -> Int -> Int
e a b c = a + b + c

f :: Int
f = 1 + 2 + 3
