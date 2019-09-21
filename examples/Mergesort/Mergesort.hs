{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Mergesort where

import Language.SPar.Skel

type T a = Either a (a, a)

msort :: CAlg f => f [Int] [Int] -> f [Int] [Int]
msort ms = (vsize &&& id) >>>
  if fst <= 1
  then snd
  else (fst &&& ((fst / 2) &&& snd))
        >>> second ((vtake >>> ms) &&& (vdrop >>> ms))
        >>> doMerge
  where
    doMerge = prim "merge"

seqMsort :: [Int] :-> [Int]
seqMsort = fix msort

parMsort :: [Int] :=> [Int]
parMsort = annotate strat $ kfix 0 msort

strat :: AnnStrat
strat = ann seqMsort
