{-# LANGUAGE CPP #-}
module CppRenameUse where

k :: Int
k = 10

-- 'e's body reads the top-level 'k'.
e :: Int -> Int
e z = z + k

-- The call site sits under a parameter also named 'k', which would capture
-- the spliced body's 'k', so inlining renames the parameter to 'k1'
-- throughout its scope. Rename information comes from the renamed source --
-- the *active* CPP branch only -- so the parameter's occurrences inside the
-- '#else' branch are invisible to the rename and keep the old name: under
-- the other CPP configuration they silently rebind to the top-level 'k'.
-- The splice and rename hunks all sit on lines away from the directives,
-- so the mangled-line vetting cannot catch this either.
f :: Int -> Int
f k =
#if 1
  let a = 0
  in e k + a
#else
  e k + k
#endif
