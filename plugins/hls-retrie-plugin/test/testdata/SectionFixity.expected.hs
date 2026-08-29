module SectionFixity where

e :: Int -> Int
e x = x * 2

-- (+) occurs only as a section operator, so 'collectOpNames' never sees
-- it and it defaults to infixl 9, which out-binds the body's (*) and makes
-- parenify wrap the spliced body in redundant parentheses.
f :: Int -> Int
f = (3 * 2 +)
