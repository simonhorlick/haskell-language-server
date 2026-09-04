module ConLabelDef where

data R = R { name :: String, n :: Int }

-- the body constructs R by its field labels
e :: String -> R
e s = R { name = s, n = 1 }
