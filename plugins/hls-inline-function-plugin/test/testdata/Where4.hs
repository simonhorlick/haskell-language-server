module Where4 where

-- Ensure a where -> let statement is correctly parenthesized.
data E = E { a :: Int }

e x = E { a = five }
  where five = 5

g x = 1

f = g (e 1)
