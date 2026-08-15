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
        concat
          [ [ 5
            , 5 + 1
            ]
          ]
   in sum xs
