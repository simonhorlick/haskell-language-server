module MultiLineLetDef where

mk :: Int -> IO Int
mk x =
  let a = x + 1
      b = x + 2
      c = case x of
        0 -> 0
        _ -> 3
  in pure (a + b + c)
