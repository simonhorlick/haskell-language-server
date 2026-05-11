module DoBlock where

foo x = Just 3

bar = do
  three <- Just 3
  -- something else
  pure $ Just three
