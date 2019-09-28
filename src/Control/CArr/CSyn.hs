{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{- LANGUAGE StandaloneDeriving #-}
{- LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE TypeFamilies #-}
{- LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.CArr.CSyn
  ( Var
  , (:<:)(..)
  , (X..)
  , vlet
  , (.$)
  , fix
--  , Fun
  , prim
  , primLit
  , cfun
  , app
  , pair
  , fst
  , snd
  , acase
  , (.|)
  , (.||)
  , inl
  , inr
  , ifThenElse
  , vget
  , vdrop
  , vtake
  , vsize
  , par
  , (@@)
  , (Prelude.$)
  , IsSing
  , FromNat
  , X.CArrCmp(..)
  , Prelude.Num(..)
  , Prelude.Fractional(..)
  , CAlg
  , X.CArrFix
  , Either
  , Int
  , TProd
  , SINat
  , pmap
  , smap
  , pfold
  , sfold
  , ssplit
  , psplit
) where

import qualified Prelude

import Data.C
import qualified Control.CCat as X
import qualified Control.CArr as X
import Control.CArr ( CAlg )

import GHC.TypeLits

class (CVal ctx, CVal ctx') => ctx :<: ctx' where
  sub :: CAlg t => t ctx' ctx

--instance {-# OVERLAPPING #-}
--  (CVal ctx) => ctx :<: ctx where
--  sub = X.id
--
--instance
--  (CVal a, ctx :<: ctx') => ctx :<: (a, ctx') where
--  sub = X.snd X.>>> sub

type family (:==:) a b where
  a :==: a = 'Prelude.True
  _ :==: _ = 'Prelude.False

class (CVal ctx, CVal ctx', ctx :==: ctx' ~ flag) => Sub flag ctx ctx' where
  subCtx :: CAlg t => t ctx' ctx

instance CVal ctx => Sub 'Prelude.True ctx ctx where
  subCtx = X.id

instance (CVal a, Sub b ctx1 ctx2, (ctx1 :==: (a, ctx2)) ~ 'Prelude.False) =>
  Sub 'Prelude.False ctx1 (a, ctx2) where
  subCtx = X.snd X.>>> subCtx

instance (CVal ctx, CVal ctx', Sub b ctx ctx') => ctx :<: ctx' where
  sub = subCtx

--instance
--  (CVal ctx, CVal ctx'', ctx :<: ctx', ctx' :<: ctx'') =>
--  ctx :<: ctx'' where
--  sub = sub X.>>> sub @ctx @ctx'

--newtype Var ctx a = Var { unVar :: forall t. CAlg t => t ctx a }
--
--var :: (CAlg t, CVal a, ctx :<: ctx') => Var ctx a -> t ctx' a
--var v = sub X.>>> unVar v

type Var t ctx a = forall ctx'. (CVal ctx', ctx :<: ctx') => t ctx' a

fix :: (CAlg t, X.CArrFix t, CVal a, CVal b)
    => Int
    -> (forall f. CAlg f => (forall ctx. CVal ctx => f ctx a -> f ctx b) ->
        Var f a a -> f a b)
    -> t a b
fix k f = X.kfix k Prelude.$ \rf -> f (app rf) sub

cfun :: (CAlg t, CVal a, CVal b) => (Var t a a -> t a b) -> t a b
cfun f = f sub

app :: (CVal a, CVal b, CVal ctx, CAlg t) => t a b -> t ctx a -> t ctx b
app f x =  f X.. x

prim :: (CAlg t, CVal a, CVal b, CVal ctx) => String -> t ctx a -> t ctx b
prim s x = X.arr s Prelude.undefined X.. x

primLit :: (CAlg t, CVal a, CVal ctx) => a -> t ctx a
primLit s = X.lit s

vlet :: forall t ctx a b. (CAlg t, CVal ctx, CVal a, CVal b)
     => t ctx a -> (Var t (a, ctx) a -> t (a, ctx) b) -> t ctx b
vlet x f = x X.&&& X.id X.>>> f (sub X.>>> (X.fst :: t (a, ctx) a))

(.$) :: (CAlg t, CVal ctx, CVal a, CVal b)
     => (Var t (a, ctx) a -> t (a, ctx) b) -> t ctx a -> t ctx b
f .$ x = vlet x f

pair :: (CAlg t, CVal ctx, CVal a, CVal b)
     => (t ctx a, t ctx b) -> t ctx (a, b)
pair (l, r) = l X.&&& r

fst :: (CAlg t, CVal ctx, CVal a, CVal b)
     => t ctx (a, b) -> t ctx a
fst f = f X.>>> X.fst

snd :: (CAlg t, CVal ctx, CVal a, CVal b)
     => t ctx (a, b) -> t ctx b
snd f = f X.>>> X.snd

docase :: (CAlg t, CVal ctx, CVal a, CVal b)
       => t ctx (Either a b) -> t ctx (Either (a, ctx) (b, ctx))
docase f = f X.&&& X.id X.>>> X.distrL

acase :: forall t ctx a b c. (CAlg t, CVal ctx, CVal a, CVal b, CVal c)
      => t ctx (Either a b)
      -> (Var t (a, ctx) a -> t (a, ctx) c)
      -> (Var t (b, ctx) b -> t (b, ctx) c)
      -> t ctx c
acase x l r = docase x X.>>>
  l (sub X.>>> (X.fst :: t (a, ctx) a)) X.|||
  r (sub X.>>> (X.fst :: t (b, ctx) b))

(.|) :: ( (Var t (a, ctx) a -> t (a, ctx) c) ->
          (Var t (b, ctx) b -> t (b, ctx) c) ->
          t ctx c )
     -> (Var t (a, ctx) a -> t (a, ctx) c)
     -> (Var t (b, ctx) b -> t (b, ctx) c)
     -> t ctx c
cse .| f = cse f

(.||) :: ( (Var t (b, ctx) b -> t (b, ctx) c) ->
           t ctx c )
     -> (Var t (b, ctx) b -> t (b, ctx) c)
     -> t ctx c
cse .|| f = cse f

inl :: (CAlg t, CVal ctx, CVal a, CVal b) => t ctx a -> t ctx (Either a b)
inl x = x X.>>> X.inl

inr :: (CAlg t, CVal ctx, CVal a, CVal b) => t ctx b -> t ctx (Either a b)
inr x = x X.>>> X.inr

ifThenElse :: (CAlg t, CVal ctx, CVal a) => t ctx Bool
           -> t ctx a -> t ctx a -> t ctx a
ifThenElse = X.ifThenElse

vget :: (CAlg t, CVal ctx, CVal a) => t ctx Int -> t ctx [a] -> t ctx a
vget i v = i X.&&& v X.>>> X.proj

vtake :: (CAlg t, CVal ctx, CVal a) => t ctx Int -> t ctx [a] -> t ctx [a]
vtake i v = i X.&&& v X.>>> X.vtake

vdrop :: (CAlg t, CVal ctx, CVal a) => t ctx Int -> t ctx [a] -> t ctx [a]
vdrop i v = i X.&&& v X.>>> X.vdrop

vsize :: (CAlg t, CVal ctx, CVal a) => t ctx [a] -> t ctx Int
vsize v = v X.>>> X.vsize

par :: (CAlg t, CVal ctx, CVal a) => (t ctx a -> t ctx b) -> t ctx a -> t ctx b
par f x = f (x X.>>> X.newProc)

(@@) :: (CAlg t, CVal ctx, CVal a) => (t ctx a -> t ctx b) -> Prelude.Integer
     -> t ctx a -> t ctx b
f @@ p = \x -> f (x X.>>> X.runAt p)

---- Pairs

data INat = Z | S INat

data family Sing :: k -> *

data instance Sing (n :: INat) where
  SZ :: Sing 'Z
  SS :: Sing n -> Sing ('S n)

class IsSing a where sing :: Sing a
instance IsSing 'Z where sing = SZ
instance IsSing n => IsSing ('S n) where sing = SS sing
type SINat (n :: INat) = Sing n

type TProd n a = Prod (FromNat (n-1)) a

type family Prod (n :: INat) (a :: *) = r where
  Prod 'Z a = a
  Prod ('S n) a = (a, Prod n a)

data CDict a where
  CDict :: CVal a => CDict a

cvalProd :: CVal a => SINat n -> t a -> CDict (Prod n a)
cvalProd SZ _ = CDict
cvalProd (SS m) t = case cvalProd m t of
                    CDict -> CDict

fmapP' :: (CAlg t, CVal a, CVal b, CVal ctx)
       => SINat n -> v a -> v b -> Bool
       -> (t ctx a -> t ctx b) -> t ctx (Prod n a) -> t ctx (Prod n b)
fmapP' SZ _ _ b f x
  | b = f (x X.>>> X.newProc)
  | Prelude.otherwise  = f x
fmapP' (SS m) tya tyb b f x =
  case (cvalProd m tya, cvalProd m tyb) of
    (CDict, CDict) ->
      let fx = if b then f (fst x X.>>> X.newProc)
               else f (fst x)
      in pair (fx, fmapP' m tya tyb b f (snd x))


--fmapPIx :: SINat n -> (Int -> a -> b) -> Int -> Prod n a -> Prod n b
--fmapPIx SZ _ _ = const unit
--fmapPIx (SS m) f k = f k *** fmapPIx m f (k+1)

pmap :: forall n t a b ctx.
        (CAlg t, CVal a, CVal b, CVal ctx, IsSing (FromNat n))
     => (t ctx a -> t ctx b)
     -> t ctx (Prod (FromNat n) a) -> t ctx (Prod (FromNat n) b)
pmap = fmapP' (sing :: SINat (FromNat n)) Proxy Proxy Prelude.True

smap :: forall n t a b ctx.
        (CAlg t, CVal a, CVal b, CVal ctx, IsSing (FromNat n))
     => (t ctx a -> t ctx b)
     -> t ctx (Prod (FromNat n) a) -> t ctx (Prod (FromNat n) b)
smap = fmapP' (sing :: SINat (FromNat n)) Proxy Proxy Prelude.False

--assocL :: (CAlg t, CVal a, CVal b, CVal c) => t (a, (b, c)) ((a, b), c)
--assocL = cfun Prelude.$ \x -> pair (pair (fst x, fst (snd x)), snd (snd x))

type family Div2 (n :: INat) where
  Div2 'Z = 'Z
  Div2 ('S 'Z) = 'Z
  Div2 ('S ('S n)) = 'S (Div2 n)

div2 :: SINat n -> SINat (Div2 n)
div2 SZ = SZ
div2 (SS SZ) = SZ
div2 (SS (SS n)) = SS (div2 n)

data Proxy a = Proxy

red :: forall t a ctx n. (CAlg t, CVal a, CVal ctx)
    => SINat n -> (t ctx (a, a) -> t ctx a)
    -> t ctx (Prod n a) -> t ctx (Prod (Div2 n) a)
red SZ _ x = x
red (SS SZ) f x = f x
red (SS (SS n)) f x =
  case (cvalProd n (Proxy :: Proxy a), cvalProd (div2 n) (Proxy :: Proxy a)) of
    (CDict, CDict) ->
      f (x X.>>> X.fst X.&&& (X.snd X.>>> X.fst)) X.&&& red n f (x X.>>> X.snd X.>>> X.snd)

pfold' :: forall t n a ctx. (CAlg t, CVal a, CVal ctx)
       => SINat n -> (t ctx (a, a) -> t ctx a)
       -> t ctx (Prod n a) -> t ctx a
pfold' SZ _ z = z
pfold' (SS SZ) f x = f x
pfold' n f x =
  case (cvalProd n (Proxy :: Proxy a), cvalProd dn2 (Proxy :: Proxy a)) of
    (CDict, CDict) -> let !rd = red n f x
                      in
                        pfold' dn2 f rd
  where
    !dn2 = div2 n

pfold :: forall n t a ctx. (CAlg t, CVal a, CVal ctx, IsSing (FromNat n))
       => (t ctx (a, a) -> t ctx a)
       -> t ctx (Prod (FromNat n) a) -> t ctx a
pfold = pfold' (sing :: SINat (FromNat n))

sfold' :: forall t n a ctx. (CAlg t, CVal a, CVal ctx)
      => SINat n -> (t ctx (a, a) -> t ctx a) -> t ctx (Prod n a) -> t ctx a
sfold' SZ _  x = x
sfold' (SS n) f x =
  case (cvalProd n (Proxy :: Proxy a)) of
    CDict -> f (pair (fst x, sfold' n f (snd x)))

sfold :: forall n t a ctx. (CAlg t, CVal a, CVal ctx, IsSing (FromNat n))
      => (t ctx (a, a) -> t ctx a) -> t ctx (Prod (FromNat n) a) -> t ctx a
sfold = sfold' (sing :: SINat (FromNat n))

split' :: forall t a ctx n. (CAlg t, CVal a, CVal ctx)
      => Bool -> SINat n -> Prelude.Integer -> (t ctx Int -> t ctx a) -> t ctx (Prod n a)
split' b SZ i g =
  if b then g (Prelude.fromInteger i) X.>>> X.newProc
  else g (Prelude.fromInteger i)
split' b (SS n) i g =
  case (cvalProd n (Proxy :: Proxy a)) of
    CDict ->
      let gi = if b then g (Prelude.fromInteger i) X.>>> X.newProc
               else g (Prelude.fromInteger i)
      in pair (gi, split' b n (i Prelude.+ 1) g)

psplit :: forall n t a ctx. (CAlg t, CVal a, CVal ctx, IsSing (FromNat n))
      => (t ctx Int -> t ctx a) -> t ctx (Prod (FromNat n) a)
psplit = split' Prelude.True (sing :: SINat (FromNat n)) 0

ssplit :: forall n t a ctx. (CAlg t, CVal a, CVal ctx, IsSing (FromNat n))
      => (t ctx Int -> t ctx a) -> t ctx (Prod (FromNat n) a)
ssplit = split' Prelude.False (sing :: SINat (FromNat n)) 0

type family ToNat (i :: INat) :: Nat where
  ToNat 'Z = 0
  ToNat ('S n) = 1 + ToNat n

type family FromNat (i :: Nat) :: INat where
  FromNat 0 = 'Z
  FromNat n = 'S (FromNat (n-1))
