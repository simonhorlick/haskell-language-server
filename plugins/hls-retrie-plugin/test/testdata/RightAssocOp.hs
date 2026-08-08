module RightAssocOp where

-- (^) is right-associative. Splicing '2 ^ x' onto the left of another '^'
-- without parens reassociates: '(2 ^ 3) ^ 4' (= 4096) and
-- '2 ^ 3 ^ 4' (= 2^81) are different programs. The plugin must keep the
-- parentheses around the inlined body in this position.
e :: Int -> Int
e x = 2 ^ x

f :: Int
f = e 3 ^ 4
