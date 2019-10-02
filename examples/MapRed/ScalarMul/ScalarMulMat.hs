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

scalarProd :: forall n f ctx. (CAlg f, CVal ctx)
           => SINat n
           -> Expr f ctx (Prod n [[Int]])
           -> Expr f ctx (Prod n [[Int]])
scalarProd n = smap @[[Int]] @[[Int]] n (par $ prim "prod")

catv :: forall n f a ctx. (IsSing n, CAlg f, CVal a, CVal ctx)
           => SINat n -> Expr f ctx (Prod n [a]) -> Expr f ctx [a]
catv n z = sfold n (prim "cat" @@ 0) z

parProd :: forall n. (IsSing n)
        => SINat n -> [[Int]] :=> [[Int]]
parProd n = cfun $ \x -> catv n $ scalarProd n $ splitv n x

-- parProd0 :: [[Int]] :=> [[Int]]
-- parProd0 = parProd @0

parProd1 :: [[Int]] :=> [[Int]]
parProd1 = withSize 1 parProd

parProd2 :: [[Int]] :=> [[Int]]
parProd2 = withSize 2 parProd

parProd4 :: [[Int]] :=> [[Int]]
parProd4 = withSize 4 parProd

parProd8 :: [[Int]] :=> [[Int]]
parProd8 = withSize 8 parProd

parProd16 :: [[Int]] :=> [[Int]]
parProd16 = withSize 16 parProd

parProd32 :: [[Int]] :=> [[Int]]
parProd32 = withSize 32 parProd

parProd64 :: [[Int]] :=> [[Int]]
parProd64 = withSize 64 parProd


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
