{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Control.CArr.CSyn
  ( (:<:)
  , (-<)
  , alet
  , var
  , pair
  , fst
  , snd
  ) where

import Data.C
import qualified Control.CCat as X
import qualified Control.CArr as X
import Control.CArr ( CAlg )

(-<) :: (CAlg t, CVal ctx, CVal a, CVal b)
     => (Var ctx a -> t ctx b) -> t ctx a -> t ctx b
f -< x = f x

alet :: (CAlg t, CVal ctx, CVal a, CVal b)
     => t ctx a
     -> (forall ctx'. ctx :<: ctx' =>
         Var ctx' a -> t ctx' b) -> t ctx b
alet x f = x X.&&& X.id X.>>> f X.fst

pair :: (CAlg t, CVal ctx, CVal a, CVal b)
     => (t ctx a, t ctx b) -> t ctx (a, b)
pair (l, r) = l X.&&& r

fst :: (CAlg t, CVal ctx, CVal a, CVal b)
     => t ctx (a, b) -> t ctx a
fst f = f X.>>> X.fst

snd :: (CAlg t, CVal ctx, CVal a, CVal b)
     => t ctx (a, b) -> t ctx b
snd f = f X.>>> X.snd

newtype Var ctx a = Var { unVar :: forall t. CAlg t => t ctx a }

class (CVal ctx, CVal ctx') => ctx :<: ctx' where
  var :: CAlg t => Var ctx a -> t ctx' a
instance CVal ctx => ctx :<: ctx where
  var p = unVar p
instance (CVal a, ctx :<: ctx') => ctx :<: (a, ctx') where
  var p = X.snd X.>>> var p
