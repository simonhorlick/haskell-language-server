module DoBlockCapture where

-- The body of 'greet' do-binds 'name'. Inlining at a call site whose
-- argument also mentions a name bound at the splice point would capture
-- the do-binder. The plugin should rename the inner do-binder.
greet :: String -> IO String
greet prefix = do
  name <- getLine
  pure (prefix ++ name)

result :: IO String
result = do
  name <- getLine
  greet name
