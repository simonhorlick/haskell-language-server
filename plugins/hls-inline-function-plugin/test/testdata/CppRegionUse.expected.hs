{-# LANGUAGE CPP #-}
module CppRegionUse where

-- 'e's multi-line body contains a blank line. The plugin builds its edits by
-- line-diffing the exact-printed parsed module, which is the *preprocessed*
-- text: the '#if 0' block below the call site is blanked there. The diff
-- aligns the body's blank line with those blanks, so a hunk lands inside the
-- CPP region -- and applied to the real document it splices body fragments
-- into the dead branch, silently changing or corrupting the code.
e :: Int
e =
  1

    + 2

f :: Int
f = e
#if 0
dead :: Int
dead = 0
#else
alive :: Int
alive = 1
#endif
