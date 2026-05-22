module A where

foo x y
| x == 0 = y
| otherwise = x + y

bar = foo 1 2
