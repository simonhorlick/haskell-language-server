{-# LANGUAGE CPP #-}
module WhereToLetCpp where

e x = a x
  where
    a x = x + 1

#if 1
-- a multi-line rewrite keeps its layout in a CPP module
f x = e x
#else
f x = e x
#endif
