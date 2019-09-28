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
module ParMap where

--import qualified Prelude as P
import Control.CArr.CSyn
--import Language.SPar.Skel ( (:=>), emptyASt, compileAsLib )
--import Control.Monad.CGen
--import System.Environment

--timesBench :: CAlg f => f (TProd 32 [Int]) (TProd 32 [Int])
--timesBench = cfun $ pmap (psize @32) (prim "sum")

scalarProd :: CAlg f => f [Int] [Int]
scalarProd = cfun $ \x ->
  vlet (vsize x / 32) $
  \sz' -> let takeFun y = vtake sz' $ vdrop (y * sz') x in
    vlet (ssplit @5 takeFun) $
    \z -> vlet (pmap @5 (prim "prod") z) $
          \t -> pfold @5 (prim "cat") t

--main :: P.IO ()
--main = withProgName "ParMap" (generateFile emptyASt "ParMap" $ P.pure () P.>> compileAsLib "scalarProd" P.mempty scalarProd)
