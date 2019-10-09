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
import Control.CCat
import Control.CArr
import Control.CArr.CSyn ( INat(..), SINat, Sing(..), CDict(..), Tree
                         , cdictTree, withCDict, toInteger, withSize )


tsplit :: forall f a n. (CAlg f, CVal a) => SINat n -> f a (a, a) -> f a (Tree n a)
tsplit SZ _f = newProc
tsplit (SS n) f =
  withCDict (cdictTree @a n) $ f >>> (tsplit n f *** tsplit n f)

tfold  :: forall f a n. (CAlg f, CVal a) => SINat n -> f (a, a) a -> f (Tree n a) a
tfold  SZ _f = id
tfold (SS n) f =
  withCDict (cdictTree @a n) $ (tfold n f *** tfold n f) >>> f

zipTree :: CAlg f => SINat n -> f ([Double], [Double]) [Double]
         -> f (Tree n [Double], Tree n [Double]) (Tree n [Double])
zipTree SZ f = f
zipTree (SS x) f =
  withCDict (cdictTree @[Double] x) $
  let swap = ((fst >>> fst) &&& (snd >>> fst)) &&&
             ((fst >>> snd) &&& (snd >>> snd))
  in swap >>> (zipTree x f *** zipTree x f)

fmapTIx :: CAlg f => SINat n -> f (Int, [Double]) [Double] -> Integer -> f (Tree n [Double]) (Tree n [Double])
fmapTIx SZ f k = Prelude.fromInteger k &&& id >>> f
fmapTIx (SS x) f k =
  withCDict (cdictTree @[Double] x) $
  fmapTIx x f k *** fmapTIx x f (k + (2 ^ (toInteger x :: Integer)))

addPadding :: CAlg f => f [Double] [Double]
addPadding = prim "add_padding"

deinterleave :: CAlg f => f [Double] ([Double], [Double])
deinterleave = prim "deinterleave"

fftTree :: CAlg f => SINat n -> f (Tree n [Double]) (Tree n [Double])
fftTree SZ = prim "baseFFT"
fftTree (SS x)
  = withCDict (cdictTree @[Double] x) $
    (fftTree x {-EVENS-} *** fftTree x {-ODDS-}) >>>
    (id *** fmapTIx x (prim "exp") 0) >>>
    -- (id *** id) -- fmapTIx x (uncurry $ mulExp p2sx) 0 {- Multiply by exponential -} >>>
    (zipTree x addc {- Left side -} &&& zipTree x subc {- right side -})
  where
    addc :: CAlg f => f ([Double], [Double]) [Double]
    addc = snd &&& fst >>> prim "zip_add"
    subc :: CAlg f => f ([Double], [Double]) [Double]
    subc = prim "zip_sub"


fft :: CAlg f => SINat n -> f [Double] [Double]
fft n =
  withCDict (cdictTree @[Double] n) $
  addPadding >>> tsplit n deinterleave >>> fftTree n >>> tfold n (runAt 0 >>> prim "cat")

fft0 :: CAlg f => f [Double] [Double]
fft0 = withSize 1 fft

fft1 :: CAlg f => f [Double] [Double]
fft1 = withSize 1 fft

fft2 :: CAlg f => f [Double] [Double]
fft2 = withSize 2 fft

fft3 :: CAlg f => f [Double] [Double]
fft3 = withSize 3 fft

fft4 :: CAlg f => f [Double] [Double]
fft4 = withSize 4 fft

fft5 :: CAlg f => f [Double] [Double]
fft5 = withSize 5 fft

fft6 :: CAlg f => f [Double] [Double]
fft6 = withSize 6 fft

fft7 :: CAlg f => f [Double] [Double]
fft7 = withSize 7 fft

fft8 :: CAlg f => f [Double] [Double]
fft8 = withSize 8 fft
  --where
  --  p2sx :: Integer
  --  p2sx = 2 ^ (fromINat (SS x) :: Integer)

--fft :: CAlg f => SINat n
--    -> f (Tree n [Double]) (Tree n [Double])

-- Below fails for some reason!
--fft :: CAlg f => SINat n
--    -> f (Tree n [Double]) (Tree n [Double])
--fft SZ = cfun $ prim "baseFFT"
--fft (SS n) = cfun $ \x ->
--  vlet (withCDict (cdictTree @[Double] n) $ app (fft n) $ fst x) $ \l ->
--  vlet (app (fft n) $ snd x) $ \r -> _
  -- vlet (par (fft n) $ fst x) $ \l -> _
--fft n = cfun $ \z ->
--  case n of
--    SZ -> prim "baseFFT" z
--    (SS x) ->
--      vlet (par (app $ fft x) $ fst z) $ \l ->
--      vlet (par (app $ fft x) $ snd z) $ \r ->
--      _
