{-# LANGUAGE ScopedTypeVariables #-}

module ImplicitForall where

-- ScopedTypeVariables brings e's forall'd 'a' into the body, so the
-- expression signature '(x :: a)' refers to it.
e :: forall a. a -> a
e x = (x :: a)

-- f has its own forall'd 'a', distinct from e's. At the call
-- site e's 'a' instantiates to 'b'. Naive inlining would produce
-- '(y :: a)' where 'a' now resolves to f's 'a' — a type error
-- since y :: b.
f :: forall a b. a -> b -> b
f _ y = e y
