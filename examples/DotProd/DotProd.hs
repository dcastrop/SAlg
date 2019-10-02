{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module DotProd where

import Control.CArr.CSyn

--timesBench :: CAlg f => f (TProd 32 [Int]) (TProd 32 [Int])
--timesBench = cfun $ pmap (psize @32) (prim "sum")

dotProdN :: forall f n. CAlg f => SINat n -> f ([Double],[Double]) Double
dotProdN i = cfun $ \x ->
  vlet (vsize (fst x) / toInt i) $ \sz' ->
  if sz' == 0 then
    prim "dot" x
  else
    let takeFun y = pair ( vtake sz' $ vdrop (y * sz') $ fst x
                         , vtake sz' $ vdrop (y * sz') $ snd x )
    in vlet (ssplit i takeFun) $
       \z -> vlet (smap i (par dot) z) $
             \t -> pfold i (cfun $ \s -> fst s + snd s) t
  where
    dot :: Expr f ctx ([Double], [Double]) -> Expr f ctx Double
    dot = prim "dot"


dotProd :: CAlg f => f ([Double],[Double]) Double
dotProd = withSize 32 $ dotProdN
