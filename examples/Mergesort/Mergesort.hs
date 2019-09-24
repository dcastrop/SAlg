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

msort :: (CAlg f, CArrFix f) => f [Int] [Int]
msort = fix 2 $ \ms x ->
  vlet (vsize x) $ \sz ->
  if sz <= 1
  then x
  else vlet (sz / 2) $ \sz2 ->
    vlet (par ms $ vtake sz2 x) $ \xl ->
    vlet (par ms $ vdrop sz2 x) $ \xr ->
    par (prim "merge") $ pair (sz, pair (xl, xr))

parMsort :: [Int] :=> [Int]
parMsort = msort

strat :: AnnStrat
strat = ann msort

type T a = Either a (a, a)

split :: (CAlg f, CArrFix f) => f [Int] (T [Int])
split = cfun $ \x ->
  vlet (vsize x) $ \sz ->
  if sz <= 1
  then inl x
  else vlet (sz / 2) $ \sz2 ->
    vlet (vtake sz2 x) $ \xl ->
    vlet (vdrop sz2 x) $ \xr ->
    inr $ (pair (xl, xr))
