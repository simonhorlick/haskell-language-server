module CaptureLocalBinding where

f :: Int -> Int
f n = inner
  where
    k = n * 2
    e z = z + k              -- inline expression; its free var `k` is local
    inner = let k = 2 in e k -- call site under a binder that shadows `k`
