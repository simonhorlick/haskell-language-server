{-# LANGUAGE LambdaCase #-}
module CaseBodyIndent where

-- 's's body is a multi-line 'case'. Spliced into a parenthesized operand
-- inside a '\case' alternative indented deeper than the definition, the
-- alternatives keep their original columns, left of the enclosing layout,
-- so the module no longer parses. Found on ghcide's showPosition
-- (HoverDefinition.hs) and 'go' (Spans/Common.hs), hls-graph's compute and
-- hls-plugin-api's callStackToSrcLoc -- the same column skew through case
-- alternatives, comprehension continuations and multi-way-if guards;
-- fixed by establishing a layout context for '\case' alternatives.
s :: Int -> String
s n = case n of
  0 -> "zero"
  _ -> "other"

render :: Int -> String
render = \case
  0 -> "none"
  n -> "at " ++ (s n) ++ "!"
