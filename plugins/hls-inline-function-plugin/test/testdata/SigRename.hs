module SigRename where

f :: Int -> Int
f n = inner
  where
    k = n * 2
    e z = z + k
    inner =
      let k :: Int
          k = 2
      in e k
