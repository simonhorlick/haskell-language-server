module DoBlock where

e x = Just 3

f = do
  three <- e 1
  -- something else
  pure $ Just three
