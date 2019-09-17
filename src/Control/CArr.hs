{-# LANGUAGE MultiParamTypeClasses #-}
module Control.CArr
  ( CArr (..)
  , CArrChoice (..)
  , CArrVec (..)
  , CArrFix (..)
  ) where

import qualified Prelude
import Prelude hiding (fst, snd, id)

import Data.C
import Control.CCat

infixr 3 ***
infixr 3 &&&
infixr 2 +++
infixr 2 |||

class CCat t => CArr t where
  arr :: (CVal a, CVal b) => String -> (a -> b) -> t a b

  fst :: (CVal a, CVal b) => t (a, b) a
  fst = arr "fst" Prelude.fst

  snd :: (CVal a, CVal b) => t (a, b) b
  snd = arr "snd" Prelude.snd

  first :: (CVal a, CVal b, CVal c) => t a b -> t (a, c) (b, c)
  first = (*** id)

  second :: (CVal a, CVal b, CVal c) => t a b -> t (c, a) (c, b)
  second = (id ***)

  (***) :: (CVal a, CVal a', CVal b, CVal b')
    => t a a' -> t b b' -> t (a, b) (a', b')
  f *** g = first f >>> arr "swap" swap >>> first g >>> arr "swap" swap
    where swap ~(x,y) = (y,x)

  (&&&) :: (CVal a, CVal b, CVal c) => t a b -> t a c -> t a (b, c)
  f &&& g = arr "dup" (\b -> (b,b)) >>> f *** g


class CArr a => CArrChoice a where

  inl :: (CVal b, CVal c) => a b (Either b c)
  inl = arr "inl" Left

  inr :: (CVal b, CVal c) => a c (Either b c)
  inr = arr "inr" Right

  left :: (CVal b, CVal c, CVal d)
       => a b c -> a (Either b d) (Either c d)
  left = (+++ id)

  right :: (CVal b, CVal c, CVal d)
        => a b c -> a (Either d b) (Either d c)
  right = (id +++)

  (+++) :: (CVal b, CVal c, CVal b', CVal c')
        => a b c -> a b' c' -> a (Either b b') (Either c c')
  f +++ g = left f >>> arr "mirror" mirror >>> left g >>> arr "mirror" mirror
    where
      mirror :: Either x y -> Either y x
      mirror (Left x) = Right x
      mirror (Right y) = Left y

  (|||) :: (CVal b, CVal c, CVal d)
        => a b d -> a c d -> a (Either b c) d
  f ||| g = f +++ g >>> arr "untag" untag
    where
      untag (Left x) = x
      untag (Right y) = y

-- No static size checking
class (Num i, CArr t) => CArrVec i t where
  proj :: CVal a => t (i, [a]) a
  vec :: (CVal a, CVal b) => t (i, a) b -> t (i, a) [b]
  vsize :: CVal a => t [a] i


class CArr t => CArrFix t where
  fix :: (CVal a, CVal b) => (t a b -> t a b) -> t a b
