module PatternLambda where

e = \a1@(Just x) -> x > 10

f = (\a1@(Just x) -> x > 10) (Just 1)

