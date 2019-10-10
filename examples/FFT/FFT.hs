{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
module FFT where

import Prelude ( Double, ($), Integer, Num(..), Int, (^) )
import qualified Prelude

import Data.C ( CVal )
import Data.Complex
import Control.CCat
import Control.CArr
import Control.CArr.CSyn ( INat(..), SINat, Sing(..), CDict(..), Tree
                         , cdictTree, withCDict, toInteger, withSize )


tsplit' :: forall f a n. (CAlg f, CVal a) => SINat n -> Int
        -> (Int -> f a (a, a))
        -> f a (Tree n a)
tsplit' SZ _acc _f = newProc
tsplit' (SS n) acc f =
  withCDict (cdictTree @a n) $ f acc >>>
  (tsplit' n (1+acc) f *** tsplit' n (1+acc) f)

tsplit :: forall f a n. (CAlg f, CVal a)
       => SINat n
       -> (Int -> f a (a, a))
       -> f a (Tree n a)
tsplit n = tsplit' n 0

tfold  :: forall f a n. (CAlg f, CVal a)
       => SINat n -> f (a, a) a -> f (Tree n a) a
tfold  SZ _f = id
tfold (SS n) f =
  withCDict (cdictTree @a n) $ (tfold n f *** tfold n f) >>> f

zipTree :: CAlg f => SINat n -> f ([Complex Double], [Complex Double]) [Complex Double]
         -> f (Tree n [Complex Double], Tree n [Complex Double]) (Tree n [Complex Double])
zipTree SZ f = f
zipTree (SS x) f =
  withCDict (cdictTree @[Complex Double] x) $
  let swap = ((fst >>> fst) &&& (snd >>> fst)) &&&
             ((fst >>> snd) &&& (snd >>> snd))
  in swap >>> (zipTree x f *** zipTree x f)

fmapTIx :: CAlg f => SINat n -> Integer
        -> f (Int, (Int, [Complex Double])) [Complex Double] -> Integer
        -> f (Tree n [Complex Double]) (Tree n [Complex Double])
fmapTIx SZ n f k = Prelude.fromInteger n &&& Prelude.fromInteger k &&& id >>> f
fmapTIx (SS x) n f k =
  withCDict (cdictTree @[Complex Double] x) $
  fmapTIx x n f k *** fmapTIx x n f (k + (2 ^ (toInteger x :: Integer)))

addPadding :: CAlg f => f [Complex Double] [Complex Double]
addPadding = prim "add_padding"

deinterleave :: CAlg f => Int -> f [Complex Double] ([Complex Double], [Complex Double])
deinterleave i = id &&& (lit (2^i) &&& id >>> vdrop)

fftTree :: forall f n. CAlg f => SINat n -> Int -> f (Tree n [Complex Double]) (Tree n [Complex Double])
fftTree SZ i = ((lit (2^i) :: f [Complex Double] Int) &&& id) >>> prim "baseFFT"
fftTree (SS x) i =
  withCDict (cdictTree @[Complex Double] x) $
  (fftTree x (i+1) {-EVENS-} *** fftTree x (i+1) {-ODDS-}) >>>
  (id *** fmapTIx x ps2x (prim "map_exp") 0) >>>
  -- (id *** id) -- fmapTIx x (uncurry $ mulExp p2sx) 0 {- Multiply by exponential -} >>>
  (zipTree x addc {- Left side -} &&& zipTree x subc {- right side -})
  where
    ps2x :: Integer
    ps2x = 2 ^ toInteger (SS x)
    addc :: CAlg f => f ([Complex Double], [Complex Double]) [Complex Double]
    addc = snd &&& fst >>> prim "zip_add"
    subc :: CAlg f => f ([Complex Double], [Complex Double]) [Complex Double]
    subc = prim "zip_sub"


fft :: CAlg f => SINat n -> f [Complex Double] [Complex Double]
fft n =
  withCDict (cdictTree @[Complex Double] n) $
  addPadding >>>
  tsplit n deinterleave >>> fftTree n 0 >>> tfold n (runAt 0 >>> prim "cat")

fft0 :: CAlg f => f [Complex Double] [Complex Double]
fft0 = withSize 1 fft

fft1 :: CAlg f => f [Complex Double] [Complex Double]
fft1 = withSize 2 fft

fft2 :: CAlg f => f [Complex Double] [Complex Double]
fft2 = withSize 2 fft

fft3 :: CAlg f => f [Complex Double] [Complex Double]
fft3 = withSize 3 fft

fft4 :: CAlg f => f [Complex Double] [Complex Double]
fft4 = withSize 4 fft

fft5 :: CAlg f => f [Complex Double] [Complex Double]
fft5 = withSize 5 fft

fft6 :: CAlg f => f [Complex Double] [Complex Double]
fft6 = withSize 6 fft

fft7 :: CAlg f => f [Complex Double] [Complex Double]
fft7 = withSize 7 fft

fft8 :: CAlg f => f [Complex Double] [Complex Double]
fft8 = withSize 8 fft
  --where
  --  p2sx :: Integer
  --  p2sx = 2 ^ (fromINat (SS x) :: Integer)

--fft :: CAlg f => SINat n
--    -> f (Tree n [Complex Double]) (Tree n [Complex Double])

-- Below fails for some reason!
--fft :: CAlg f => SINat n
--    -> f (Tree n [Complex Double]) (Tree n [Complex Double])
--fft SZ = cfun $ prim "baseFFT"
--fft (SS n) = cfun $ \x ->
--  vlet (withCDict (cdictTree @[Complex Double] n) $ app (fft n) $ fst x) $ \l ->
--  vlet (app (fft n) $ snd x) $ \r -> _
  -- vlet (par (fft n) $ fst x) $ \l -> _
--fft n = cfun $ \z ->
--  case n of
--    SZ -> prim "baseFFT" z
--    (SS x) ->
--      vlet (par (app $ fft x) $ fst z) $ \l ->
--      vlet (par (app $ fft x) $ snd z) $ \r ->
--      _
