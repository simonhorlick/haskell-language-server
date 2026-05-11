module DoBlock where

foo x = Just 3

bar = do
  three <- foo 1
  -- something else
  pure $ Just three
