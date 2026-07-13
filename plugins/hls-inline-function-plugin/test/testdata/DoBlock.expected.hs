module DoBlock where

e x = Just 3

f = do
  three <- Just 3
  -- something else
  pure $ Just three
