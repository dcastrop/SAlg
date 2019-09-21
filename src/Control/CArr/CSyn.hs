{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Control.CArr.CSyn
  ( Var
  , (:<:)(..)
  , alet
  , ($)
  , var
  , fun
  , pair
  , fst
  , snd
  , acase
  , inl
  , inr
  ) where

-- import qualified Prelude

import Data.C
import qualified Control.CCat as X
import qualified Control.CArr as X
import Control.CArr ( CAlg )

newtype Var ctx a = Var { unVar :: forall t. CAlg t => t ctx a }

class (CVal ctx, CVal ctx') => ctx :<: ctx' where
  sub :: CAlg t => t ctx' ctx

instance CVal ctx => ctx :<: ctx where
  sub = X.id

instance (CVal a, ctx :<: ctx') => ctx :<: (a, ctx') where
  sub = X.snd X.>>> sub

var :: (CAlg t, CVal a, ctx :<: ctx') => Var ctx a -> t ctx' a
var v = sub X.>>> unVar v

type Fun t ctx a b = forall ctx'. ctx :<: ctx' => Var ctx' a -> t ctx' b

alet :: (CAlg t, CVal ctx, CVal a, CVal b)
     => t ctx a -> Fun t ctx a b -> t ctx b
alet x f = x X.&&& X.id X.>>> f (Var X.fst)

($) :: (CAlg t, CVal ctx, CVal a, CVal b)
     => Fun t ctx a b -> t ctx a -> t ctx b
f $ x = alet x f

fun :: (CAlg t, CVal ctx, CVal a, CVal b) => t a b -> Var ctx a -> t ctx b
fun f x = unVar x X.>>> f

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
