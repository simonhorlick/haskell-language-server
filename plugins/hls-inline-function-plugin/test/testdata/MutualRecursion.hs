module MutualRecursion where

ping :: Int -> Int
ping x = pong x

pong :: Int -> Int
pong x = ping x

result :: Int
result = ping 5
