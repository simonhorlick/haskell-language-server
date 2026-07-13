module Pattern where

-- A single clause pattern match
e (Just x) = x > 10

f = e (Just 1)
