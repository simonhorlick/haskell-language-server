{-# LANGUAGE MultiWayIf #-}
module MultiWayIfDef (grade) where

-- The body is a multi-way 'if', which parses only because this module
-- enables MultiWayIf.
grade :: Int -> String
grade n = if | n > 0     -> "pos"
             | otherwise -> "nonpos"
