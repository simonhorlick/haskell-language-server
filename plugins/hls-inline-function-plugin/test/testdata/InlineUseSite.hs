module InlineUseSite where

e :: Int -> Int
e x = x + 1

main :: Int
main = e 1 + e 2
