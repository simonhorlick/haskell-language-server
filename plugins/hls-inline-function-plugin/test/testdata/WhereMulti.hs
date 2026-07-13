module WhereMulti where

mk :: Int -> IO ()
mk x = print (pick x + bonus)
  where pick 0 = 0
        pick n = n + bonus
        bonus = 7

f :: IO ()
f = do
    putStrLn "start"
    mk 3
