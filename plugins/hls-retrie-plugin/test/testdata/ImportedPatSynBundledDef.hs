{-# LANGUAGE PatternSynonyms #-}
module ImportedPatSynBundledDef (T (Zero), e) where

newtype T = T Int

pattern Zero :: T
pattern Zero = T 0

-- the body names a pattern synonym bundled with its type
e :: Int -> T
e n = Zero
