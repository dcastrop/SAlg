{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module MapRed where

import Control.CArr.CSyn

--timesBench :: CAlg f => f (TProd 32 [Int]) (TProd 32 [Int])
--timesBench = cfun $ pmap (psize @32) (prim "sum")

dotProd :: CAlg f => f ([Int],[Int]) [Int]
dotProd = cfun $ \x ->
  vlet (vsize (fst x) / 4) $ \sz' ->
  if sz' == 0 then
    prim "dot" x
  else let takeFun y = pair ( vtake sz' $ vdrop (y * sz') $ fst x
                            , vtake sz' $ vdrop (y * sz') $ snd x ) in
    vlet (ssplit @3 takeFun) $ \z ->
    vlet (smap @3 (\s -> par (prim "dot") s) z) $ \t ->
    pfold @3 (\s -> par (prim "sum") $ s) t
    -- pfold @3 (prim "sum") t
