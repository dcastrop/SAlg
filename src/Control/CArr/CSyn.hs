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
{- LANGUAGE TypeApplications #-}
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
  , X.CArrCmp(..)
  , Prelude.Num(..)
  , Prelude.Fractional(..)
  , CAlg
  , X.CArrFix
  , Either
  , Int
) where

import qualified Prelude

import Data.C
import qualified Control.CCat as X
import qualified Control.CArr as X
import Control.CArr ( CAlg )

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
