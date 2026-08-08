module OpChain where

e :: Int -> Int
e y = y * 2

-- The argument 1 + 2 * 3 parses left-nested as (1 + 2) * 3. The AST must be
-- re-associated with the in-scope fixities before rewriting or parenify reads
-- the wrong top operator off the substituted chain and drops the required
-- parentheses.
f :: Int
f = e (1 + 2 * 3)
