module LayoutDeep where

items :: Int -> [Int]
items n =
  concat
    [ [ n
      , n + 1
      ]
    ]

use :: Int
use =
  let xs =
        items 5
   in sum xs
