module ArgRename where

-- The call-site binder 'y' (f's parameter) would capture e's free 'y':
-- the call site is refused and the file left unchanged (capture-renaming
-- lives on the capture-rename posterity branches).
y :: Int
y = 0

e :: Int -> Int
e x = x + y

f :: Int -> Int
f y = e (y + 1)
