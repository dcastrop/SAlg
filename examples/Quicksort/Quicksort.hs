{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Quicksort where

import Control.CArr.CSyn
import Language.SPar.Skel ( (:=>) )

filter :: forall f ctx. (CAlg f, CVal ctx)
       => Expr f ctx [Int] -> Expr f ctx ([Int], [Int])
filter = prim "filter"

qsort :: forall f. (CAlg f, CArrFix f) => Int -> f [Int] [Int]
qsort n = fix n $ \qs x ->
  if vsize x <= 1
  then x
  else vlet (filter x) $ \lr ->
    vlet (par qs $ fst lr) $ \xl ->
    vlet (qs $ snd lr) $ \xr ->
    prim "cat" @@ 0 $ pair (xl, xr)

parMsort0 :: [Int] :=> [Int]
parMsort0 = qsort 0

parMsort1 :: [Int] :=> [Int]
parMsort1 = qsort 1

parMsort2 :: [Int] :=> [Int]
parMsort2 = qsort 2

parMsort3 :: [Int] :=> [Int]
parMsort3 = qsort 3

parMsort4 :: [Int] :=> [Int]
parMsort4 = qsort 4

parMsort5 :: [Int] :=> [Int]
parMsort5 = qsort 5

parMsort6 :: [Int] :=> [Int]
parMsort6 = qsort 6

parMsort7 :: [Int] :=> [Int]
parMsort7 = qsort 7

parMsort8 :: [Int] :=> [Int]
parMsort8 = qsort 8

parMsort9 :: [Int] :=> [Int]
parMsort9 = qsort 9
