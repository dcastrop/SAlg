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
parMsort = annotate strat $ kfix 4 msort

strat :: AnnStrat
strat = ann seqMsort
