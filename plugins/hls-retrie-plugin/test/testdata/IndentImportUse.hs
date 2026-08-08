module IndentImportUse where

  import CrossFileDef (e)
  -- the whole module body is indented: the appended import must land at
  -- the layout column and restore it for the pushed declaration
  f :: Int
  f = e (Just 7)
