module ArgParens where

e :: (Int -> Int) -> Int -> Int
e p q = p q

-- Pass an App node to e (negate 3), this *must* have parenthesis added
-- or the resulting substitution of e into f will have incorrect
-- precedence
f :: Int
f = succ `e` negate 3
