module ArgRename where

-- The call-site binder 'y' (f's parameter) would capture e's free 'y', so it
-- is renamed. The renamed binder also appears *inside the argument* '(y + 1)',
-- so the alpha-rename must reach into the argument expression too.
y :: Int
y = 0

e :: Int -> Int
e x = x + y

f :: Int -> Int
f y1 = y1 + 1 + y
