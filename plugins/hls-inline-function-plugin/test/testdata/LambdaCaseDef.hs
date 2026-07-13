{-# LANGUAGE LambdaCase #-}
module LambdaCaseDef (classify, plainShow) where

-- The body is a '\case' lambda, which parses only because this module
-- enables LambdaCase.
classify :: Int -> String
classify = \case
  0 -> "zero"
  _ -> "other"

-- A '\case'-free body from the same module: enabling the extension here must
-- not by itself make the definition unavailable at targets that lack it.
plainShow :: Int -> String
plainShow n = "n: " ++ show n
