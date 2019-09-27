{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Main where

import qualified Prelude as P
import Control.CArr.CSyn
import Language.SPar.Skel ( (:=>), emptyASt, compileAsLib )
import Control.Monad.CGen
import System.Environment

--timesBench :: CAlg f => f (TProd 32 [Int]) (TProd 32 [Int])
--timesBench = cfun $ pmap (psize @32) (prim "sum")

scalarProd :: [Int] :=> [Int]
scalarProd = cfun $ \x ->
  vlet (vsize x / 32) $
  \sz' -> let takeFun y = vtake sz' $ vdrop (y * sz') x in
    vlet (psplit @15 takeFun) $
    \z -> vlet (smap @15 (prim "prod") z) $
          \t -> pfold @15 (prim "cat" @@ 0) t

main :: P.IO ()
main = withProgName "ParMap" (generateFile emptyASt "ParMap" $ P.pure () P.>> compileAsLib "scalarProd" P.mempty scalarProd)
