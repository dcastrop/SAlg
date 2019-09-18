{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators #-}
module Mergesort where

import Language.SPar.Skel

type T a = Either a (a, a)

msort :: CAlg f => f [Int] [Int] -> f [Int] [Int]
msort ms =
  if vsize .<= 1
  then id
  else (vtake (vsize / 2) >>> ms) &&&
       (vdrop (vsize / 2) >>> ms)
       >>> prim "merge"

seqMsort :: [Int] :-> [Int]
seqMsort = fix msort

parMsort :: [Int] :=> [Int]
parMsort = annotate strat $ kfix 1 msort

strat :: AnnStrat
strat = ann seqMsort

--mrg :: CAlg f => f (T [Int]) [Int]
--mrg =
--  id ||| prim "merge"
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
