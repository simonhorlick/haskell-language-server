module CaseBranch where

addOne :: Int -> Int
addOne x = x + 1

result :: Maybe Int -> Int
result m = case m of
  Just n  -> addOne n
  Nothing -> 0
