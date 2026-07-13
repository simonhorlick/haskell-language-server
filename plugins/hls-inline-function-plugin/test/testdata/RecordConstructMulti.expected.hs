{-# LANGUAGE LambdaCase #-}
module RecordConstructMulti where

-- 'mkR's body is a record construction laid out over several lines with the
-- opening brace on the line after the constructor. Inlining rewrites two
-- sites in the '\case' below, both indented deeper than the definition:
-- the head of a multi-line record update and an update inside a let-in.
-- The spliced construction's brace lines keep their original columns, left
-- of the enclosing layout, so the module no longer parses ("parse error on
-- input '{'"). Found on ghcide's FindImports 'notFound'; fixed by
-- establishing a layout context for '\case' alternatives in exactprint.
data R = R
  { ra :: [Int]
  , rb :: Maybe Int
  , rc :: [Int]
  }

useIt :: Int -> [R]
useIt x = map go [x]
  where
    go =
      \case
        0 ->
          R
          { ra = []
          , rb = Nothing
          , rc = []
          }
             { rc = [1]
             , rb = Nothing
             }
        v ->
          let y = [v]
              w = y
              u =
                [length w]
           in R
            { ra = []
            , rb = Nothing
            , rc = []
            } {ra = u}

mkR :: R
mkR = R
  { ra = []
  , rb = Nothing
  , rc = []
  }
