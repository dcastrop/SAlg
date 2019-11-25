{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Mergesort where

import Control.CArr.CSyn
import Language.SPar.Skel ( (:=>), AnnStrat, annotate, ann )
import qualified Language.SPar.Skel as S


--merge1 :: CAlg f => f ([Double], [Double]) [Double]
--merge1 = prim "merge"
--
--merge2 :: CAlg f => f ([Double], [Double]) [Double]
--merge2 = cfun $ \x -> merge1 . x
--
--test1 :: CAlg f => f [Double] [Double]
--test1 = cfun $ \x ->
--  if vsize x <= 1
--  then x
--  else x
--
--test2 :: CAlg f => f [Double] [Double]
--test2 = cfun $ \x ->
--  vlet (vsize x) $ \sz ->
--  if sz <= 1
--  then x
--  else x
--
--parTest :: [Double] :=> [Double]
--parTest = annotate (ann (S.vsize :: [Double] :-> Double)) $ test2

msort :: (CVal a, CAlg f) => Int -> f [a] [a]
msort n = fix n $ \ms x ->
  vlet (vsize x) $ \sz ->
  if sz <= 1
  then x
  else vlet (sz / 2) $ \sz2 ->
    vlet (par ms $ vtake sz2 x) $ \xl ->
    vlet (par ms $ vdrop sz2 x) $ \xr ->
    app merge $ pair (sz, pair (xl, xr))

merge :: (CVal a, CAlg f, CArrFix f) => f (Int, ([a], [a])) [a]
merge = cfun $  prim "merge"
--
-- msort :: (CAlg f, CArrFix f) => Int -> f [Double] [Double]
-- msort n = pfix n $ \ms x ->
--   vlet (vsize x) $ \sz ->
--   if sz <= 1
--   then x
--   else vlet (sz / 2) $ \sz2 ->
--     vlet (ms $ vtake sz2 x) $ \xl ->
--     vlet (ms $ vdrop sz2 x) $ \xr ->
--     prim "merge" $ pair (sz, pair (xl, xr))

msort_2 :: (CAlg f, CArrFix f) => f [Double] [Double]
msort_2 = fix 2 $ \ms x ->
  vlet (vsize x) $ \sz ->
  if sz <= 1
  then x
  else vlet (sz / 2) $ \sz2 ->
    vlet (par ms $ vtake sz2 x) $ \xl ->
    vlet (ms $ vdrop sz2 x) $ \xr ->
    (prim "merge") $ pair (sz, pair (xl, xr))

parMsort0 :: [Double] :=> [Double]
parMsort0 = msort 0

parMsort1 :: [Double] :=> [Double]
parMsort1 = msort 1

parMsort1a :: [Double] :=> [Double]
parMsort1a = msort_2

parMsort2 :: [Double] :=> [Double]
parMsort2 = msort 2

parMsort3 :: [Double] :=> [Double]
parMsort3 = msort 3

parMsort4 :: [Double] :=> [Double]
parMsort4 = msort 4

parMsort5 :: [Double] :=> [Double]
parMsort5 = msort 5

parMsort6 :: [Double] :=> [Double]
parMsort6 = msort 6

parMsort7 :: [Double] :=> [Double]
parMsort7 = msort 7

parMsort8 :: [Double] :=> [Double]
parMsort8 = msort 8

--strat :: AnnStrat
--strat = ann msort
--
--type T a = Either a (a, a)
--
--split :: (CAlg f, CArrFix f) => f [Double] (T [Double])
--split = cfun $ \x ->
--  vlet (vsize x) $ \sz ->
--  if sz <= 1
--  then inl x
--  else vlet (sz / 2) $ \sz2 ->
--    vlet (vtake sz2 x) $ \xl ->
--    vlet (vdrop sz2 x) $ \xr ->
--    inr $ (pair (xl, xr))
