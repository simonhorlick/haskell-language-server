{-# LANGUAGE CPP #-}
module Cpp where

e :: Int
#if 1
e = 2
#else
e = 3
#endif

f :: Int
f = 2 + 1
