module PatternLambda where

e = \a1@(Just x) -> x > 10

f = e (Just 1)

