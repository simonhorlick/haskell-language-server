module CaseBranch where

e :: Int -> Int
e x = x + 1

f :: Maybe Int -> Int
f m = case m of
  Just n  -> n + 1
  Nothing -> 0
