{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Mergesort where

import Control.CArr.CSyn
import Language.SPar.Skel ( (:=>), AnnStrat, annotate, ann )
import qualified Language.SPar.Skel as S


--merge1 :: CAlg f => f ([Int], [Int]) [Int]
--merge1 = prim "merge"
--
--merge2 :: CAlg f => f ([Int], [Int]) [Int]
--merge2 = cfun $ \x -> merge1 . x
--
--test1 :: CAlg f => f [Int] [Int]
--test1 = cfun $ \x ->
--  if vsize x <= 1
--  then x
--  else x
--
--test2 :: CAlg f => f [Int] [Int]
--test2 = cfun $ \x ->
--  vlet (vsize x) $ \sz ->
--  if sz <= 1
--  then x
--  else x
--
--parTest :: [Int] :=> [Int]
--parTest = annotate (ann (S.vsize :: [Int] :-> Int)) $ test2

msort :: (CAlg f, CArrFix f) => Int -> f [Int] [Int]
msort n = fix n $ \ms x ->
  vlet (vsize x) $ \sz ->
  if sz <= 1
  then x
  else vlet (sz / 2) $ \sz2 ->
    vlet (par ms $ vtake sz2 x) $ \xl ->
    vlet (par ms $ vdrop sz2 x) $ \xr ->
    (par $ prim "merge") $ pair (sz, pair (xl, xr))

msort_2 :: (CAlg f, CArrFix f) => f [Int] [Int]
msort_2 = fix 2 $ \ms x ->
  vlet (vsize x) $ \sz ->
  if sz <= 1
  then x
  else vlet (sz / 2) $ \sz2 ->
    vlet (par ms $ vtake sz2 x) $ \xl ->
    vlet (ms $ vdrop sz2 x) $ \xr ->
    (prim "merge") $ pair (sz, pair (xl, xr))

parMsort0 :: [Int] :=> [Int]
parMsort0 = msort 0

parMsort1 :: [Int] :=> [Int]
parMsort1 = msort 1

parMsort1a :: [Int] :=> [Int]
parMsort1a = msort_2

parMsort2 :: [Int] :=> [Int]
parMsort2 = msort 2

parMsort3 :: [Int] :=> [Int]
parMsort3 = msort 3

parMsort4 :: [Int] :=> [Int]
parMsort4 = msort 4

parMsort5 :: [Int] :=> [Int]
parMsort5 = msort 5

parMsort6 :: [Int] :=> [Int]
parMsort6 = msort 6

parMsort7 :: [Int] :=> [Int]
parMsort7 = msort 7

parMsort8 :: [Int] :=> [Int]
parMsort8 = msort 8

--strat :: AnnStrat
--strat = ann msort
--
--type T a = Either a (a, a)
--
--split :: (CAlg f, CArrFix f) => f [Int] (T [Int])
--split = cfun $ \x ->
--  vlet (vsize x) $ \sz ->
--  if sz <= 1
--  then inl x
--  else vlet (sz / 2) $ \sz2 ->
--    vlet (vtake sz2 x) $ \xl ->
--    vlet (vdrop sz2 x) $ \xr ->
--    inr $ (pair (xl, xr))
