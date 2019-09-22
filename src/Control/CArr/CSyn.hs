{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE IncoherentInstances #-}

module Control.CArr.CSyn
  ( Var
  , (:<:)(..)
  , alet
  , (.$)
  , var
  , Fun
  , cfun
  , (.@)
  , pair
  , fst
  , snd
  , acase
  , inl
  , inr
  , ifThenElse
  , vget
  , vdrop
  , vtake
  , vsize
  , (Prelude.$)
  , X.CArrCmp(..)
  , Prelude.Num(..)
  , Prelude.Fractional(..)
  , CAlg
  , Either
  , Int
) where

import qualified Prelude

import Data.C
import qualified Control.CCat as X
import qualified Control.CArr as X
import Control.CArr ( CAlg )

newtype Var ctx a = Var { unVar :: forall t. CAlg t => t ctx a }

class (CVal ctx, CVal ctx') => ctx :<: ctx' | ctx' -> ctx where
  sub :: CAlg t => t ctx' ctx

instance CVal ctx => ctx :<: ctx where
  sub = X.id

instance (CVal a, ctx :<: ctx') => ctx :<: (a, ctx') where
  sub = X.snd X.>>> sub

instance
  (CVal ctx, CVal ctx'', ctx :<: ctx', ctx' :<: ctx'') =>
  ctx :<: ctx'' where
  sub = sub X.>>> sub @ctx @ctx'

var :: (CAlg t, CVal a, ctx :<: ctx') => Var ctx a -> t ctx' a
var v = sub X.>>> unVar v

type Fun t ctx a b = forall ctx'. ctx :<: ctx' => Var ctx' a -> t ctx' b

cfun :: (CAlg t, CVal a, CVal b) => Fun t a a b -> t a b
cfun f = f (Var X.id)

alet :: (CAlg t, CVal ctx, CVal a, CVal b)
     => t ctx a -> Fun t ctx a b -> t ctx b
alet x f = x X.&&& X.id X.>>> f (Var X.fst)

(.$) :: (CAlg t, CVal ctx, CVal a, CVal b)
     => Fun t ctx a b -> t ctx a -> t ctx b
f .$ x = alet x f

(.@) :: (CAlg t, CVal a, CVal b) => t a b -> forall ctx. CVal ctx => t ctx a -> t ctx b
f .@ x = x X.>>> f

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

acase :: (CAlg t, CVal ctx, CVal a, CVal b, CVal c)
      => t ctx (Either a b) -> Fun t ctx a c -> Fun t ctx b c -> t ctx c
acase x l r = docase x X.>>> l (Var X.fst) X.||| r (Var X.fst)

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
