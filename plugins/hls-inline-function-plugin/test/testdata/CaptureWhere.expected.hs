module CaptureWhere where

-- The body of addY references the top-level y and the where clause for result
-- binds a local y that shadows the top-level one.
--
-- When 'addY x' is inlined, the spliced 'y' must continue to refer to the
-- top-level y.
y :: Int
y = 1

addY :: Int -> Int
addY x = x + y

result :: Int -> Int
result x = x + y + y1
  where y1 = 100
