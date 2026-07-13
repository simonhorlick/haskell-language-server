{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}
module WildcardConstruct where

data Wrap = Wrap { doc :: Int }
-- Strict fields: 'Rec{..}' may omit a non-strict field (a -Wmissing-fields
-- warning ghcide tolerates), but omitting a strict one is a hard error.
data Rec  = Rec  { doc :: !Int, other :: !Int }

-- The inlined body binds 'doc' through its own RecordWildCards parameter and
-- uses it. Two parameters, so a call with plain-variable arguments dispatches
-- as a tuple: @case (k, w) of (k, Wrap{..}) -> k + doc@.
combine :: Int -> Wrap -> Int
combine k Wrap{..} = k + doc

-- The call site binds a local 'doc' that feeds a RecordWildCards construction.
-- retrie's capture analysis once missed the dispatch's own 'Wrap{..}' binding
-- 'doc' (wildcard binders are implicit), treating the spliced body's 'doc' as
-- free and flagging the outer 'doc' as capturing: under refusal semantics
-- that would wrongly refuse this site (and under capture-renaming it broke
-- the 'Rec{..}' construction). Pattern binders resolve through the renamed
-- source, so the dispatch binder shadows correctly and the inline proceeds.
mk :: Int -> Wrap -> Rec
mk k w =
  let doc   = 7
      other = combine k w
  in Rec{..}
