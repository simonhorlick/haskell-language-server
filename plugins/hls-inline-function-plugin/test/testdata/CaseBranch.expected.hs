module CaseBranch where

addOne :: Int -> Int
addOne x = x + 1

result :: Maybe Int -> Int
result m = case m of
  Just n  -> n + 1
  Nothing -> 0
