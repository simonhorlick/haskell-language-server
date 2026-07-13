{-# LANGUAGE NamedFieldPuns #-}
module PunSelectorCapture where

data Bench = Bench { name :: Int }

-- The body references the top-level field selector 'name'.
total :: Bench -> Int
total b = name b + 1

-- The call site is enclosed by a NamedFieldPuns binder 'name' that shadows
-- the selector, so splicing the body bare silently rebinds its 'name'
-- reference to the pun binder. retrie must rename the enclosing binder to
-- keep the graft capture-free, and since 'Bench{name}' is a field pun
-- whose one token is both field label and binder, the rename must expand
-- the pun to 'Bench{name = name1}' rather than renaming the field label.
g :: Bench -> Int
g b@Bench{name} = name + total b
