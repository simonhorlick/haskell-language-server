module Let3 where

-- As in Capture.hs, trying to inline e into the let body would break the
-- meaning of the expression. This should be prevented.
f :: Int -> Int
f y = let e = y in let y = 1 in y + e
