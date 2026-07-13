module MultiClauseCon where

e :: Maybe Int -> Int
e Nothing  = 0
e (Just x) = x + 1

a :: Int
a = e Nothing

b :: Int
b = e (Just 5)
