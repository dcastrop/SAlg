{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Language.SPar.Skel
  ( SProc (..)
  , CProc
  , printSProc
  , PM
  , (:=>)(..)
  , gather
  , AnnStrat
  , ann
  , printASkel
  , compileAsLib
  , annotate
  , ($)
  , Semigroup (..)
  , Monoid (..)
  , module X
  ) where

import qualified Prelude
import Prelude hiding ( (.), fst, snd, id, const )

import Control.Monad.RWS.Strict hiding ( lift, ap, fix )
import qualified Data.Set as Set

import Data.Ratio (numerator, denominator)

import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as Map

import Control.CCat as X
import Control.CArr as X
import Language.Alg as X
import Control.Monad.CGen
import Language.SPar
import Language.SPar.Skel.Internal

newtype AnnStrat = AnnStrat { unAnnStrat :: [AAlg] }

ann :: (CVal a, CVal b) => a :-> b -> AnnStrat
ann f = AnnStrat $ AAlg f : []

instance Semigroup AnnStrat where
  AnnStrat l <> AnnStrat r = AnnStrat $ l ++ r

instance Monoid AnnStrat where
  mempty = AnnStrat []
  mappend = (<>)

inStrat :: (CVal a, CVal b) => a :-> b -> AnnStrat -> Bool
inStrat f x = AAlg f `elem` unAnnStrat x

data Defs = Defs { defm :: Map String AAlg }

emptyDefs :: Defs
emptyDefs = Defs Map.empty

data St = St { nextPID :: PID, defs :: Defs }

emptySt :: St
emptySt = St 1 emptyDefs

incPID :: St -> St
incPID st = st { nextPID = nextPID st + 1 }

-- Reader monad: [Alg] is annotation strategy
-- State monad: next PID
type PM = RWS AnnStrat () St

data SProc v t a = SProc { unProc :: Env v t, outR :: DType a }

type CProc a = SProc Alg (:->) a

--generateLibrary :: (CVal a, CVal b)
--                => AnnStrat -> a :=> b -> IO ()
--generateLibrary st f = do
--  pn <- getProgName
--  generateFile (pn ++ ".c") $ genLib f

domTy :: CVal a => a :=> b -> CTy a
domTy _ = getCTy

codTy :: CVal b => a :=> b -> CTy b
codTy _ = getCTy

compileAsLib :: (CVal a, CVal b) => String -> AnnStrat -> a :=> b -> CGen ASt ()
compileAsLib fpn st f = do
  mapM_ (uncurry declF) df
  declareEnv fpn p
  fns <- mapM (wrapParts fpn) pds
  av <- freshVar
  aty <- cTySpec $ domTy f
  bty <- cTySpec $ codTy f
  newFun (internalIdent fpn, bty) [(av, aty)]
    $ body fpn av 0 $ zip pds fns
  newHeaderFun (internalIdent fpn) bty [aty]
  where
    pds = filter (/= 0) $ domEnvL p
    declF :: String -> AAlg -> CGen ASt ()
    declF fn (AAlg af) = declareFun fn af
    (SProc p _, St _ (Defs (Map.toList -> df)), ()) =
      runRWS (unSkel (f >>> gather 0) $ DVal 0 getCTy) st emptySt

wrapParts :: String -> PID -> CGen ASt Ident
wrapParts fpn p = do
  fn <- freshN $ "fun_thread_" ++ show p
  vn <- freshN "arg"
  newFun (fn, ([CTypeSpec $ CVoidType undefNode], [CPtrDeclr [] undefNode]))
    [(vn, ([CTypeSpec $ CVoidType undefNode], [CPtrDeclr [] undefNode]))]
    [ CBlockStmt $ cExpr $
      CCall (cVar $ internalIdent $ fpn ++ "_part_" ++ show p) [] undefNode
    , CBlockStmt $ CReturn (Just $ cVar $ internalIdent "NULL") undefNode
    ]
  pure fn


body :: String -> Ident -> PID -> [(PID, Ident)] -> [CBlockItem]
body fpn av p0 ps = map declThread ps ++ map createThread ps ++ retn
  where
    retn = [ CBlockStmt $ (`CReturn` undefNode) $ Just $
             CCall (cVar $ internalIdent $ fpn ++ "_part_" ++ show p0)
             [cVar av] undefNode
           ]
    createThread (p, fn) = CBlockStmt $ cExpr $
      CCall pthreadCreate
      [ cAddr $ cVar $ threadName p
      , cVar $ internalIdent "NULL"
      , cVar fn
      , cVar $ internalIdent "NULL"
      ] undefNode
    declThread (p, _) = CBlockDecl $ CDecl
      [CTypeSpec $ CTypeDef (internalIdent "pthread_t") undefNode]
      [mkThreadDeclr p] undefNode
    mkThreadDeclr p =
      ( Just $ CDeclr (Just $ threadName p) [] Nothing [] undefNode
      , Nothing, Nothing)
    threadName p = internalIdent $ "thread" ++ show p
    pthreadCreate = cVar $ internalIdent "pthread_create"

printSProc :: CProc a -> String
printSProc p = printEnv $ unProc p

newtype (:=>) a b = SSkel { unSkel :: DType a -> PM (CProc b) }

annotate :: AnnStrat -> a :=> b -> a :=> b
annotate st sk = SSkel $ \a -> local (\_ -> st) $ unSkel sk a

printASkel :: CVal a => AnnStrat -> a :=> b -> IO ()
printASkel st (SSkel f) = do
  putStrLn $ printDefs $ Map.toList $ defm $ defs df
  putStrLn $ printSProc pc
  where
    (pc, df, ()) = runRWS (f $ DVal 0 getCTy) st emptySt

    printDefs :: [(String, AAlg)] -> String
    printDefs [] = ""
    printDefs ((d, AAlg e) : es) =
      d ++ " = " ++ printAlg 0 (unFun e) ++ "\n\n" ++ printDefs es

idSkel :: CVal a => a :=> a
idSkel = SSkel $ \x -> pure (SProc emptyEnv x)

pproc :: DType a -> CEnv -> PM (CProc a)
pproc o e = pure $ SProc { unProc = e, outR = o }

pipeline :: a :=> b -> b :=> c -> a :=> c
pipeline q p = SSkel $ \x -> do
  eq <- unSkel q x
  ep <- unSkel p (outR eq)
  pproc (outR ep) $ kleisliEnv (unProc eq) (unProc ep)

instance CCat (:=>) where
  id  = idSkel
  (.) = flip pipeline

anyPID :: DType a -> PID
anyPID i = head $ partsL i

annot :: (CVal a, CVal b) => PID -> a :-> b -> PM PID
annot i f = do
  b <- reader (f `inStrat`)
  if b then gets nextPID <* modify incPID else pure i

--getName :: a :-> b -> PM String
--getName f = do
--  nms <- Map.keysSet Prelude.. defm <$> gets defs
--  pure $ go nms Nothing
--  where
--    go nms Nothing
--      | nf `Set.member` nms = go nms (Just (0 :: Integer))
--      | otherwise = nf
--    go nms (Just i)
--      |  nfi `Set.member` nms = go nms (Just $ i+1)
--      | otherwise = nfi
--      where
--        nfi = nf ++ "_" ++ show i
--    nf = encName f

--newDef :: (CVal a, CVal b) => a :-> b -> PM String
----newDef (Fun (Prim f _)) = pure f
--newDef f = do
--  df <- gets defs
--  case Map.lookup (AAlg f) (revd df) of
--    Just n -> pure n
--    Nothing -> do
--      nm <- getName f
--      modify $ \s -> s { defs = df { defm = Map.insert nm (AAlg f) $ defm df
--                                   , revd = Map.insert (AAlg f) nm $ revd df
--                                   }
--                       }
--      pure nm

gather :: CVal a => PID -> a :=> a
gather p = SSkel $ \i -> pproc (DVal p getCTy) $ msg i p

gatherNew :: CVal a => a :=> a
gatherNew = SSkel $ \i -> do
  p <- gets nextPID <* modify incPID
  pproc (DVal p getCTy) $ msg i p

lift :: (CVal a, CVal b) => a :-> b -> a :=> b
lift f = SSkel $ \i -> do
  p <- annot (anyPID i) f
  pproc (DVal p getCTy) $ kleisliEnv (msg i p) (singleton i p $ \v -> run f v)

fstSkel :: (CVal a, CVal b) => (a, b) :=> a
fstSkel = SSkel $ \i -> pproc (ifst i) $ efst i (partsL i)

sndSkel :: (CVal a, CVal b) => (a, b) :=> b
sndSkel = SSkel $ \i -> pproc (isnd i) $ esnd i (partsL i)

--------------------------------------------------------------------------------
-- REFACTOR BELOW INTO INTERNAL
extendProc :: DType b -> [PID] -> CProc a -> CProc a
extendProc i ps (SProc e o) = SProc (extendEnv pst e) o
  where
    pst = map (\p -> (p, projTy i p)) ps

ppartsL :: CProc a -> [PID]
ppartsL (SProc e _) = domEnvL e

agreeDom :: DType c -> CProc a -> CProc b -> CProc a
agreeDom i ea eb = extendProc i (ppartsL eb ++ partsL i) ea
--------------------------------------------------------------------------------

splitProc :: (CVal a, CVal b, CVal c)
          => DType a -> CProc b -> CProc c -> CProc (b, c)
splitProc ir pl pr =
  SProc { unProc = splitEnv (participants out) (unProc plr) (unProc prl)
        , outR = out
        }
  where
    plr = agreeDom ir pl pr
    prl = agreeDom ir pr pl
    out = DPair (outR plr) (outR prl)

splitSkel :: (CVal a, CVal b, CVal c) => a :=> b -> a :=> c -> a :=> (b, c)
splitSkel f g = SSkel $ \i -> splitProc i <$> unSkel f i <*> unSkel g i

instance CArr (:=>) where
  arr nm f = lift (arr nm f)
  fst = fstSkel
  snd = sndSkel
  f *** g = (f . fst) &&& (g . snd)
  (&&&) = splitSkel

inlSkel :: forall a b. (CVal a, CVal b) => a :=> Either a b
inlSkel = SSkel $ \i -> pproc (DTagL i getCTy) emptyEnv

inrSkel :: forall a b. (CVal a, CVal b) => b :=> Either a b
inrSkel = SSkel $ \i -> pproc (DTagR getCTy i) emptyEnv

updatePID :: MonadState St m => PID -> m ()
updatePID p = get >>= \x -> put x { nextPID = p }

freezePIDs :: MonadState St m => m a -> m a
freezePIDs m = gets nextPID >>= (m <*) Prelude.. updatePID

parState :: MonadState St m => m a -> m a -> m (a, a)
parState m1 m2 = do
  (x1, s1) <- freezePIDs ((,) <$> m1 <*> gets nextPID)
  (x2, s2) <- (,) <$> m2 <*> gets nextPID
  updatePID (max s1 s2) *> pure (x1, x2)

caseSkel :: forall a b c. (CVal a, CVal b, CVal c)
         => a :=> c -> b :=> c -> Either a b :=> c
caseSkel f g = SSkel $ \i ->
  case i of
    DTagL l _   -> unSkel f l
    DTagR _ r   -> unSkel g r
    DAlt  p l r -> do
      (el, er) <- parState (unSkel (caseSkel f g) l) (unSkel (caseSkel f g) r)
      pure $ choice p l r el er
    DVal p (CEither l r) -> do
      let j = DAlt p (DTagL (DVal p l) getCTy) (DTagR getCTy (DVal p r))
      unSkel (caseSkel f g) j

choice :: (CVal a, CVal c)
       => PID
       -> DType a
       -> DType a
       -> CProc c
       -> CProc c
       -> CProc c
choice p il ir (SProc l lo) (SProc r ro) =
  SProc { unProc = kleisliEnv (choiceEnv p il ir ps) e
        , outR = o
        }
  where
    e = caseEnv ps (kleisliEnv l $ einl lo ro po) (kleisliEnv r $ einr lo ro po)
    o = DAlt p lo ro
    po = participants o
    ps = domEnv l `Set.union` domEnv r

instance CArrChoice (:=>) where
  inl = inlSkel
  inr = inrSkel
  f +++ g = (inl . f) ||| (inr . g)
  f ||| g = caseSkel f g

sif :: CVal a => (Bool, a) :=> Either a a
sif = lift mif

instance CArrIf (:=>) where
  ifThenElse test l r = test &&& id >>> sif >>> l ||| r

sfun :: (CVal a, CVal b) => (Alg a -> Alg b) -> a :=> b
sfun f = lift $ fun f

instance (CVal a, CVal b, Num b) => Num (a :=> b) where
  f + g = (f &&& g) >>> (sfun $ \v -> afst v + asnd v)
  f * g = (f &&& g) >>> (sfun $ \v -> afst v * asnd v)
  abs f = f >>> (sfun $ \v -> abs v)
  signum f = f >>> (sfun $ \v -> signum v)
  negate f = f >>> (sfun $ \v -> negate v)
  fromInteger i = sfun $ \_ -> fromInteger i

instance (CVal a, CVal b, Num b) => Fractional (a :=> b) where
  f / g = (f &&& g) >>> (sfun $ \v -> afst v / asnd v)
  recip x = 1 / x
  fromRational x = fromInteger (numerator x) / fromInteger (denominator x)

instance CArrCmp (:=>) where
  f < g = f &&& g >>>  (lift $ fst X.<  snd)
  f <= g = f &&& g >>> (lift $ fst X.<= snd)
  f >= g = f &&& g >>> (lift $ fst X.>= snd)
  f > g = f &&& g >>>  (lift $ fst X.>  snd)
  f == g = f &&& g >>> (lift $ fst X.== snd)

-- FIXME: sequential so far
instance CArrVec (:=>) where
  proj = sfun $ \v -> Proj (afst v) (asnd v)
  vsize = sfun $ \v -> VLen v
  vec _f = undefined
  vtake = sfun (\v -> VTake (afst v) (asnd v))
  vdrop = sfun (\v -> VDrop (afst v) (asnd v))

instance CArrFix (:=>) where
  fix f = lift $ fix f
  kfix k f
    | k Prelude.<= 0 = lift (Fun $ Fix f)
  kfix k f = f (kfix (k-1) f)

instance CArrPar (:=>) where
  newProc f = gatherNew >>> f
  runAt f p = gather p >>> f
