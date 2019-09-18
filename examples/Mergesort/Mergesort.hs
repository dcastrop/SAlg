{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RebindableSyntax #-}
module Mergesort where

import Language.SPar.Skel

type T a = Either a (a, a)

spl :: CAlg f => f [Int] [Int]
spl = vtake vsize
--  if vsize .< 0
--  then inl
--  else vtake (vsize / 2) &&& vdrop (vsize / 2) >>> inr

--spl :: CAlg f => f [Int] (T [Int])
--spl =
--  if vsize .< 0
--  then inl
--  else vtake (vsize / 2) &&& vdrop (vsize / 2) >>> inr
--
--          vsize &&& id >>> prim "split"
--
--mrg :: CArr f => f (T [Int]) [Int]
--mrg = prim "merge"
--
--ms :: CArrChoice t => t [Int] [Int] -> t [Int] [Int]
--ms msr = spl >>> (id +++ msr *** msr) >>> mrg
--
--msort :: [Int] :-> [Int]
--msort = fix ms
--
--pms :: [Int] :=> [Int]
--pms = annotate annot $ kfix 4 ms
--  where
--    annot = ann spl <> ann msort
