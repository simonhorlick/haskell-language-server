module IndentImportUse where

  import CrossFileDef (e)
  import Data.Maybe (fromMaybe)
  -- the whole module body is indented: the appended import must land at
  -- the layout column and restore it for the pushed declaration
  f :: Int
  f = fromMaybe 0 (Just 7)
