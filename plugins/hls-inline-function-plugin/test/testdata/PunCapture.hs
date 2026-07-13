{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
module PunCapture where

data Bench = Bench { name :: Int }

-- The inlined body binds 'name' through its own RecordWildCards parameter and
-- uses it. Two parameters, so a call with plain-variable arguments dispatches
-- as a tuple: @case (k, b) of (k, Bench{..}) -> k + name@.
runBench :: Int -> Bench -> Int
runBench k Bench{..} = k + name

-- The call site is enclosed by a NamedFieldPuns pattern that also binds 'name'.
-- retrie's capture analysis does not see the dispatch's own 'Bench{..}' bind
-- 'name' (wildcard binders are implicit), so it treats the spliced body's
-- 'name' as free and renames the outer binder to avoid capture -- but
-- 'Bench{name}' is a field pun, and the rename rewrites it to the invalid
-- 'Bench{name1}' (field 'name1' does not exist) instead of expanding it to
-- 'Bench{name = name1}'.
g :: Int -> Bench -> Int
g k b@Bench{name} = name + runBench k b
