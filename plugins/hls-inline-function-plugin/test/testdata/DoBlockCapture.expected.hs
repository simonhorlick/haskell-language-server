module DoBlockCapture where

-- The body of 'e' do-binds 'name'. Inlining at a call site whose
-- argument also mentions a name bound at the splice point would capture
-- the do-binder. The plugin should rename the inner do-binder.
e :: String -> IO String
e prefix = do
  name <- getLine
  pure (prefix ++ name)

f :: IO String
f = do
  name <- getLine
  do
    name1 <- getLine
    pure (name ++ name1)
