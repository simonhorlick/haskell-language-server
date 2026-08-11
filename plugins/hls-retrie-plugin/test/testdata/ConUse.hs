module ConUse where

import ConDef

g :: Int -> Wrapped
g n = Wrap n

h :: Wrapped -> Int
h w = case w of
  Wrap v -> v
