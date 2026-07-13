module RecordUpdateHead where

data R = R { a :: Int, b :: Int }

e :: Int -> R
e n = R x 2 where
  x = n

f :: Maybe R
f = Just (e 1)
    { b = 3
    }
