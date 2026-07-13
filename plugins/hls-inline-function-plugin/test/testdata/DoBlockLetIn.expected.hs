module DoBlockLetIn where

e :: Int -> IO Int
e x =
  let y = 5
  in pure y

f :: IO Int
f = do
  putStrLn "hi"
  -- splicing a bare let..in statement into a do block would cause
  -- a parse error on the "in", so we wrap the expression in parenthesis
  (let y = 5
   in pure y)
