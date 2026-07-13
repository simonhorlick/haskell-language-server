module CommentBody where

-- The body's first line is a '--' line comment, before the expression.
e :: Int -> Int
e x =
  -- a leading comment on the body
  x + 1

f :: Int
f = -- a leading comment on the body
  5 + 1
