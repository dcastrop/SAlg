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

zipTree :: forall a b c f n. (CAlg f, CVal a, CVal b, CVal c)
        => SINat n -> f (a, b) c
        -> f (Tree n a, Tree n b) (Tree n c)
zipTree SZ f = f
zipTree (SS x) f =
  withCDict (cdictTree @a x) $
  withCDict (cdictTree @b x) $
  withCDict (cdictTree @c x) $
  let swap = ((fst >>> fst) &&& (snd >>> fst)) &&&
             ((fst >>> snd) &&& (snd >>> snd))
  in swap >>> (zipTree x f *** zipTree x f)

type D a = (a, a)

fmapTIx :: CAlg f => SINat n -> Int -> Int
        -> f (Int, D [Complex Double]) (D [Complex Double])
        -> f (Tree n (D [Complex Double])) (Tree n (D [Complex Double]))
fmapTIx SZ _ n f = Prelude.fromIntegral n &&& id >>> f
fmapTIx (SS x) i n f =
  withCDict (cdictTree @(D [Complex Double]) x) $
  fmapTIx x (i+1) n f *** fmapTIx x (i+1) (n + (2 ^ i)) f

-- addPadding :: CAlg f => f (D [Complex Double]) (D [Complex Double])
-- addPadding = prim "add_padding"

deinterleave :: CAlg f => Int -> f (D [Complex Double]) (D [Complex Double], D [Complex Double])
deinterleave i = id &&& ((lit (2^i) &&& fst >>> udrop) &&& (lit (2^i) &&& snd >>> udrop))
  where
    udrop :: CAlg f => f (Int, [Complex Double]) [Complex Double]
    udrop = prim "udrop"

fftTree :: forall f n. CAlg f => SINat n -> Int -> Int
        -> f (Tree n (D [Complex Double])) (Tree n (D [Complex Double]))
fftTree SZ i _offset
  = ((lit (2^i) :: f (D [Complex Double]) Int) &&& id) >>> prim "baseFFT"
fftTree (SS x) i offset
  = withCDict (cdictTree @(D [Complex Double]) x) $
    (fftTree x (i+1) offset {-EVENS-} *** fftTree x (i+1) (offset + 2 ^ i) {-ODDS-}) >>>
    (fmapTIx x (i+1) offset (prim "map_exp") *** fmapTIx x (i+1) (offset + 2^i) (prim "map_exp")) >>>
    (zipTree x addc {- Left side -} &&& zipTree x subc {- right side -})
  where
    --ps2x :: Integer
    --ps2x = 2 ^ toInteger (SS x)
    addc :: CAlg f => f (D [Complex Double], D [Complex Double]) (D [Complex Double])
    addc = snd &&& fst >>> prim "zip_add"
    subc :: CAlg f => f (D [Complex Double], D [Complex Double]) (D [Complex Double])
    subc = prim "zip_sub"

fft :: CAlg f => SINat n -> f (D [Complex Double]) (D [Complex Double])
fft n =
  withCDict (cdictTree @(D [Complex Double]) n) $
  -- addPadding >>>
  tsplit n deinterleave >>> fftTree n 0 0 >>> tfold n (prim "cat")

fft0 :: CAlg f => f (D [Complex Double]) (D [Complex Double])
fft0 = withSize 0 fft

fft1 :: CAlg f => f (D [Complex Double]) (D [Complex Double])
fft1 = withSize 1 fft

fft2 :: CAlg f => f (D [Complex Double]) (D [Complex Double])
fft2 = withSize 2 fft

fft3 :: CAlg f => f (D [Complex Double]) (D [Complex Double])
fft3 = withSize 3 fft

fft4 :: CAlg f => f (D [Complex Double]) (D [Complex Double])
fft4 = withSize 4 fft

fft5 :: CAlg f => f (D [Complex Double]) (D [Complex Double])
fft5 = withSize 5 fft

fft6 :: CAlg f => f (D [Complex Double]) (D [Complex Double])
fft6 = withSize 6 fft
--
-- fft7 :: CAlg f => f (D [Complex Double]) (D [Complex Double])
-- fft7 = withSize 7 fft
--
-- fft8 :: CAlg f => f (D [Complex Double]) (D [Complex Double])
-- fft8 = withSize 8 fft
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
