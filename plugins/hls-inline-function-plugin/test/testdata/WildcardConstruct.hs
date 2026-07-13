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
-- retrie's capture analysis does not see the dispatch's own 'Wrap{..}' bind
-- 'doc' (wildcard binders are implicit), so it treats the spliced body's 'doc'
-- as free and renames the outer 'doc' to 'doc1' to avoid capture. The 'Rec{..}'
-- construction reads its 'doc' field from a variable spelled 'doc', so the
-- rename leaves the field unsupplied and the module no longer typechecks
-- ("Constructor 'Rec' does not have the required strict field(s) doc").
mk :: Int -> Wrap -> Rec
mk k w =
  let doc   = 7
      other = combine k w
  in Rec{..}
