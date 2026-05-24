module Let3 where

-- As in Capture.hs, trying to inline x into the let body would break the
-- meaning of the expression. This should be prevented.
result :: Int -> Int
result y = let y' = 1 in y' + y
