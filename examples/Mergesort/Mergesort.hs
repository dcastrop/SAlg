{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Mergesort where

import Control.CArr.CSyn
import Language.SPar.Skel ( (:->), (:=>), AnnStrat, prim, fix, annotate, kfix, ann )

-- type T a = Either a (a, a)

merge :: CAlg f => f ([Int], [Int]) [Int]
merge = cfun $ \x -> prim "merge" .@ var x

msort :: CAlg f => f [Int] [Int] -> f [Int] [Int]
msort ms = cfun $ \x ->
  alet (vsize $ var x) $ \sz ->
  if var sz <= 1
  then var x
  else
    alet (var sz / 2) $ \sz2 ->
    alet (ms .@ vtake (var sz2) (var x)) $ \xl ->
    alet (ms .@ vdrop (var sz2) (var x)) $ \xr ->
    merge .@ pair (var xl, var xr)


--msort :: CAlg f => f [Int] [Int] -> f [Int] [Int]
--msort ms = (vsize &&& id) >>>
--  if fst <= 1
--  then snd
--  else (fst &&& ((fst / 2) &&& snd))
--        >>> second ((vtake >>> ms) &&& (vdrop >>> ms))
--        >>> doMerge
--  where
--    doMerge = prim "merge"

seqMsort :: [Int] :-> [Int]
seqMsort = fix msort

parMsort :: [Int] :=> [Int]
parMsort = annotate strat $ kfix 0 msort

strat :: AnnStrat
strat = ann seqMsort
