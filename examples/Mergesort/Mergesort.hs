{-# LANGUAGE TypeOperators #-}
module Mergesort where

import Prelude hiding ( id )
import Language.SPar.Skel

type T a b = Either (Either () a) (b, b)

spl :: CArr f => f [Int] (T Int [Int])
spl = arr "split" undefined

mrg :: CArr f => f (T Int [Int]) [Int]
mrg = arr "merge" undefined

ms :: CArrChoice t => t [Int] [Int] -> t [Int] [Int]
ms msr = spl >>> (id +++ msr *** msr) >>> mrg

msort :: [Int] :-> [Int]
msort = fix ms

pms :: [Int] :=> [Int]
pms = annotate annot $ kfix 0 ms
  where
    annot = ann spl <> ann msort
