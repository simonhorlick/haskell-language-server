module ArgParens where

combine :: (Int -> Int) -> Int -> Int
combine p q = p q

-- Pass an App node to combine (negate 3), this *must* have parenthesis added
-- or the resulting substitution of combine into result will have incorrect
-- precedence
result :: Int
result = succ `combine` negate 3
