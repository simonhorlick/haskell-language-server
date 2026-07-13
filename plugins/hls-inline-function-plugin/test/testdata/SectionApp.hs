module SectionApp where

add :: Int -> Int -> Int
add x y = x + y

two :: Int
two = (`add` 2) 5
