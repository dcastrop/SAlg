{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Mergesort where

import Control.CArr.CSyn
import Language.SPar.Skel ( (:=>), AnnStrat, annotate, ann )
-- import qualified Language.SPar.Skel as S

-- type T a = Either a (a, a)

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
--  alet (vsize x) $ \sz ->
--  if sz <= 1
--  then x
--  else x
--
--parTest :: [Int] :=> [Int]
--parTest = annotate (ann (S.vsize :: [Int] :-> Int)) $ test2

msort :: (CAlg f, CArrFix f) => f [Int] [Int]
msort = fix 2 $ \ms x ->
  alet (vsize x) $ \sz ->
  if sz <= 1
  then x
  else alet (sz / 2) $ \sz2 ->
    alet (ms $ vtake sz2 x) $ \xl ->
    alet (ms $ vdrop sz2 x) $ \xr ->
    prim "merge" $ pair (sz, pair (xl, xr))

parMsort :: [Int] :=> [Int]
parMsort = annotate strat $ msort

strat :: AnnStrat
strat = ann msort
