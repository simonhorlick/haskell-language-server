module QualifiedCtorDef (Wrapped(..), unwrap) where

-- 'unwrap' matches its argument with a constructor pattern.
data Wrapped = Wrapped Int

unwrap :: Wrapped -> Int
unwrap (Wrapped n) = n + 1
