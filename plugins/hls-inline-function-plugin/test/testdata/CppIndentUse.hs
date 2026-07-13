{-# LANGUAGE CPP #-}
module CppIndentUse where

-- This file uses CPP, so its edits are vetted against the real document
-- text: hunks may only touch lines the preprocessed print shares with the
-- document. The '#if' block sits away from the call site, so this
-- multi-line splice's hunks stay on clean lines and apply through the
-- whole-module reprint, keeping the continuation aligned under the splice
-- point. (A textual splice of the replacement would drop the continuation
-- at column zero and break the layout -- the failure the soak found.)
#if 1
flag :: Bool
flag = True
#endif

e :: Int -> Int
e x =
  x
    + 1

f :: Int -> Int
f n =
  let y = e n
  in y + 1
