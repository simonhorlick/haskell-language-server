{-# LANGUAGE Haskell2010       #-}
-- FlexibleInstances admits the concrete instance head; FlexibleContexts
-- stays off, so an inferred non-type-variable constraint is rejected.
{-# LANGUAGE FlexibleInstances #-}
module SigDischargeUse where

data Wrap a = Wrap a

class Pretty a where
  pp :: a -> String

instance Pretty (Wrap Int) where
  pp _ = "wrap"

-- 'render's signature fixes the argument to Int, so 'Pretty (Wrap Int)' is
-- solved at its instance and no constraint escapes the definition.
render :: Int -> String
render n = pp (Wrap n)

-- Inlining 'render x' into the signature-less 'go' makes GHC re-infer go's
-- type from the spliced body 'pp (Wrap x)': the constraint 'Pretty (Wrap a)'
-- floats out with a non-type-variable argument, which Haskell2010 rejects
-- ("Non type-variable argument in the constraint... Perhaps you intended to
-- use the FlexibleContexts extension"). The definition's signature was
-- load-bearing: it discharged the constraint the splice re-opens. Found on
-- retrie's mkVarPat inlined into Subst.unpunRenamedFields. (expectFail
-- until fixed.)
useIt :: Int -> String
useIt n = go n
  where
    go x = render x
