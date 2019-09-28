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
module ScalarMulMat where

--import qualified Prelude as P
import Control.CArr.CSyn
import Language.SPar.Skel ( (:=>) )
--import Control.Monad.CGen
--import System.Environment

--timesBench :: CAlg f => f (TProd 32 [Int]) (TProd 32 [Int])
--timesBench = cfun $ pmap (psize @32) (prim "sum")

scalarProd :: forall n f. (CValProd n [[Int]], CValProd n [Int], CAlg f)
           => f (TProd n [[Int]]) (TProd n [[Int]])
scalarProd = cfun $ \z -> smap @n @[[Int]] @[[Int]] (par $ prim "prod") z

catv :: forall n f a. (CValProd n [a], CAlg f, CVal a)
           => f (TProd n [a]) [a]
catv = cfun $ \z -> sfold @n (prim "cat" @@ 0) z

parProd :: [[Int]] :=> [[Int]]
parProd = cfun $ \x -> catv @11 `app` scalarProd @11 `app` splitv @11 x


--  cfun $ \i ->
--  vlet (app prd i) $ \j ->
--  pfold @5 (par $ app cat) j
--  where
--    prd = cfun $ \x ->
--      vlet (vsize x / 32) $
--      \sz' -> let takeFun y = vtake sz' $ vdrop (y * sz') x in
--        vlet (ssplit @5 takeFun) $
--        \z -> smap @5 (prim "prod") z
--    cat = cfun $ prim "cat"
--
--scalarSeq :: [Int] :-> [Int]
--scalarSeq = scalarProd
--
--scalarPar :: [Int] :=> [Int]
--scalarPar = lift scalarProd

--main :: P.IO ()
--main = withProgName "ParMap" (generateFile emptyASt "ParMap" $ P.pure () P.>> compileAsLib "scalarProd" P.mempty scalarProd)
