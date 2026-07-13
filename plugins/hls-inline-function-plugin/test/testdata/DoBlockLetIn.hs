module DoBlockLetIn (retryBusy, runIt) where

retryBusy :: Int -> IO Int
retryBusy action =
  let isBusy e
        | e > 0     = Just e
        | otherwise = Nothing
  in
    pure (maybe action id (isBusy action))

runIt :: IO Int
runIt = do
  putStrLn "start"
  retryBusy 5
