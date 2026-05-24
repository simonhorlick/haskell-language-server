{-# LANGUAGE ScopedTypeVariables #-}

module ImplicitForall where

-- ScopedTypeVariables brings helper's forall'd 'a' into the body, so the
-- expression signature '(x :: a)' refers to it.
helper :: forall a. a -> a
helper x = (x :: a)

-- result has its own forall'd 'a', distinct from helper's. At the call
-- site helper's 'a' instantiates to 'b'. Naive inlining would produce
-- '(y :: a)' where 'a' now resolves to result's 'a' — a type error
-- since y :: b.
result :: forall a b. a -> b -> b
result _ y = helper y
