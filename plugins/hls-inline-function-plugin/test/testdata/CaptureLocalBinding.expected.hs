module CaptureLocalBinding where

f :: Int -> Int
f n = inner
  where
    k = n * 2
    e z = z + k              -- inline expression; its free var `k` is local
    inner = let k1 = 2 in k1 + k -- call site under a binder that shadows `k`
