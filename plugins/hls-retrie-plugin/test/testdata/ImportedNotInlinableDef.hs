module ImportedNotInlinableDef where

data R = MkR { field :: Int }

class C a where
  m :: a -> a

instance C Int where
  m = id
