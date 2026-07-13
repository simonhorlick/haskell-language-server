module SectionApp where

add :: Int -> Int -> Int
add x y = x + y

two :: Int
two = (\x -> x + 2) 5
