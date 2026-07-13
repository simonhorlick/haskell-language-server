{-# LANGUAGE NamedFieldPuns #-}
module PunSelectorCapture where

data Bench = Bench { name :: Int }

-- The body references the top-level field selector 'name'.
total :: Bench -> Int
total b = name b + 1

-- The call site is enclosed by a NamedFieldPuns binder 'name' that shadows
-- the selector: splicing the body bare would silently rebind its 'name'
-- reference to the pun binder. The site is refused and the file left
-- unchanged. Detecting this requires the rename index to record selector
-- occurrences (an XExpr HsRecSelRn node in the renamed AST), not just
-- HsVar ones.
g :: Bench -> Int
g b@Bench{name} = name + total b
