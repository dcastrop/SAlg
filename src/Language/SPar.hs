{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
module Language.SPar
  ( SPar (..)
  , CPar
  , printCPar
  , PID
  , DType (..)
  , participants
  , partsL
  , projTy
  , ifst
  , isnd
  , ATy (..)
  , ATyFn (..)
  , TyFun (..)
  , commStruct
  , pprSType
  , getTyRep
  , send
  , recv
  , trecv
  , run
  , mCse
  , branch
  , select
  , isInl
  , isInr
  , simpl
  , codeGen
  , declareParFun
  ) where

import Type.Reflection

import Control.Monad.CGen
import Data.C
import Language.Alg

import qualified Data.Set as Set
import           Data.Set  ( Set )

data SPar v t a where
  MSnd :: CVal a
       => v a
       -> PID
       -> SPar v t b
       -> SPar v t b

  MRcv :: CVal a
       => PID
       -> TypeRep a
       -> (v a -> SPar v t b)
       -> SPar v t b

  MRun :: (CVal a, CVal b)
       => t a b
       -> v a
       -> (v b -> SPar v t c)
       -> SPar v t c

  MCse :: (CVal a, CVal b, CVal c)
       => v (Either a b)
       -> (v a -> SPar v t (v c))
       -> (v b -> SPar v t (v c))
       -> (v c -> SPar v t d)
       -> SPar v t d

  MIf :: CVal a
       => v Bool
       -> SPar v t (v a)
       -> SPar v t (v a)
       -> (v a -> SPar v t b)
       -> SPar v t b

  MRet :: a
       -> SPar v t a

isVar :: Alg t -> Integer -> Bool
isVar (BVar i) j | i Prelude.== j = True
isVar _ _ = False

isCase :: CPar a -> Bool
isCase (MCse l kl kr k)
  | isVar l 0 = True
  | otherwise = isCase (kl (BVar 1))
                || isCase (kr (BVar 1))
                || isCase (k (BVar 1))
isCase (MSnd _ _ k) = isCase k
isCase (MRcv _ _ k) = isCase (k (BVar 1))
isCase (MRun _ _ k) = isCase (k (BVar 1))
isCase _ = False


isInl :: CPar a -> Bool
isInl = pRet iil
  where
    iil (Inl _) = True
    iil _ = False

isInr :: CPar a -> Bool
isInr = pRet iir
  where
    iir (Inr _) = True
    iir _ = False

pRet :: (Alg a -> Bool) -> CPar a -> Bool
pRet p (MRet x) = p x
pRet p (MCse _ _ _ k) = pRet p (k (BVar 0))
pRet p (MRun _ _ k) = pRet p (k (BVar 0))
pRet p (MRcv _ _ k) = pRet p (k (BVar 0))
pRet p (MSnd _ _ k) = pRet p k
pRet p (MIf _ _ _ k) = pRet p (k (BVar 0))

printCPar :: Integer -> CPar a -> String
printCPar l (MSnd v p k)
  = "send " ++ printAlg l v ++ " " ++ show p ++ " ;\n" ++ printCPar l k
printCPar l (MRcv p _ k)
  = "recv " ++ show p ++ " ; /?" ++ show l ++ ". \n "
  ++ printCPar (l+1) (k (BVar l))
printCPar l (MRun _ x k)
  = "run " ++ "(" ++ printAlg l x
  ++ ") ; /?" ++ show l ++ ". \n" ++ printCPar (l+1) (k (BVar l))
printCPar l (MCse v x y k)
  = "case (" ++ printAlg l v ++ ") \n " ++
    "(/?" ++ show l ++ ". " ++ printCPar (l+1) (x (BVar l)) ++ ") \n" ++
    "(/?" ++ show l ++ ". " ++ printCPar (l+1) (y (BVar l)) ++ ") ; /?"
    ++ show l ++ ". \n " ++ printCPar (l+1) (k (BVar l))
printCPar l (MIf v x y k)
  = "if (" ++ printAlg l v ++ ") \n " ++
    "(/?" ++ show l ++ ". " ++ printCPar l x ++ ") \n" ++
    "(/?" ++ show l ++ ". " ++ printCPar l y ++ ") ; /?"
    ++ show l ++ ". \n " ++ printCPar (l+1) (k (BVar l))
printCPar l (MRet x) = "end " ++ printAlg l x

newtype TyFun a b = TyFun { unTyFun :: TypeRep (a -> b) }

getType :: CVal a => v a -> TypeRep a
getType _ = typeRep

getTyFun :: (CVal a, CVal b) => t a b -> TyFun a b
getTyFun _ = TyFun typeRep

type SType a = SPar TypeRep TyFun (TypeRep a)

commStruct :: (CVal a, CAp t v) => SPar v t (v a) -> SType a
commStruct (MSnd v p k) = MSnd (getType v) p (commStruct k)
commStruct (MRcv p t k) = MRcv p t (\_ -> commStruct (k var))
commStruct (MRun f v k) =
  MRun (getTyFun f) (getType v) (\_ -> commStruct (k var))
commStruct (MCse v kl kr k) =
  MCse (getType v)
    (\_ -> commStruct (kl var))
    (\_ -> commStruct (kr var))
    (\_ -> commStruct (k var))
commStruct (MIf v kl kr k) =
  MIf (getType v)
    (commStruct kl)
    (commStruct kr)
    (\_ -> commStruct (k var))
commStruct (MRet _) = MRet typeRep

pprSType :: SType a -> String
pprSType (MSnd v p k) =
  show p ++ " ! <" ++ show v ++ ">; " ++
  pprSType k
pprSType (MRcv p t k) =
  show p ++ " ? (" ++ show t ++ "); " ++
  pprSType (k typeRep)
pprSType (MRun f v k) =
  "{ " ++ show (unTyFun f) ++ " } " ++ show v ++ "; " ++
  pprSType (k typeRep)
pprSType (MCse v kl kr k) =
  "case (" ++ show v ++ ") (" ++
  pprSType (kl typeRep) ++ " | " ++
  pprSType (kr typeRep) ++ "); " ++
  pprSType (k typeRep)
pprSType (MIf _v kl kr k) =
  "if (" ++
  pprSType kl ++ " | " ++
  pprSType kr ++ "); " ++
  pprSType (k typeRep)
pprSType (MRet v) =
  "end (" ++ show v ++ ")"


type CPar a = SPar Alg (:->) (Alg a)

send :: CVal a => Alg a -> PID -> CPar ()
send v p = MSnd v p (MRet (Lit ()))

bcast :: CVal a => Alg a -> [PID] -> CPar ()
bcast _ []     = MRet (Lit ())
bcast v (p:ps) = MSnd v p $ bcast v ps


data DType a where
  DPair  :: (CVal a, CVal b)
         => DType a
         -> DType b
         -> DType (a, b)

  DTagL :: (CVal a, CVal b)
        => DType a
        -> CTy b
        -> DType (Either a b)

  DTagR :: (CVal a, CVal b)
        => CTy a
        -> DType b
        -> DType (Either a b)

  -- DVec :: CVal a => [DType a] -> DType [a]

  DVal :: CVal a => PID -> CTy a -> DType a
  DAlt :: CVal a => PID -> DType a -> DType a -> DType a

getTyRep :: DType a -> TypeRep a
getTyRep (DPair _ _) = typeRep
getTyRep (DTagL _ _) = typeRep
getTyRep (DTagR _ _) = typeRep
getTyRep (DVal _ _) = typeRep
getTyRep (DAlt _ _ _) = typeRep

partsL :: DType a -> [PID]
partsL = Set.toList . participants

participants :: DType a -> Set PID
participants (DAlt p l r) =
  Set.unions [Set.singleton p, participants l, participants r]
-- participants (DVec ps) = Set.unions $ map participants ps
participants (DVal p _) = Set.singleton p
participants (DTagL p _) = participants p
participants (DTagR _ p) = participants p
participants (DPair l r) = participants l `Set.union` participants r

ifst :: (CVal a, CVal b) => DType (a, b) -> DType a
ifst (DVal p (CPair l _)) = DVal p l
ifst (DPair l _) = l
ifst (DAlt p l r) = DAlt p (ifst l) (ifst r)

isnd :: (CVal a, CVal b) => DType (a, b) -> DType b
isnd (DVal p (CPair _ r)) = DVal p r
isnd (DPair _ r) = r
isnd (DAlt p l r) = DAlt p (isnd l) (isnd r)

data ATy where
  ATy :: CVal a => CTy a -> ATy

pairATy :: ATy -> ATy -> ATy
pairATy (ATy l) (ATy r) = ATy $ CPair l r

eitherATy :: ATy -> ATy -> ATy
eitherATy (ATy l) (ATy r) = ATy $ CEither l r

instance Eq ATy where
  ATy l == ATy r = case eqTypeRep (getCTyR l) (getCTyR r) of
                     Just _ -> True
                     Nothing -> False

data ATyFn where
  ATyFn :: (CVal a, CVal b) => TypeRep a -> TypeRep b -> ATyFn

projTy :: DType a -> PID -> ATy
projTy i p
  | p `Set.notMember` participants i = ATy CUnit
projTy (DPair l r) p = pairATy (projTy l p) (projTy r p)
projTy (DTagL i _) p = projTy i p
projTy (DTagR _ i) p = projTy i p
projTy (DVal pt t) p
  | p Prelude.== pt = ATy t
  | otherwise = ATy CUnit
projTy i@(DAlt _ l r) p
  | p `Set.member` participants i = eitherATy tl tr
  | otherwise = if tl Prelude.== tr then tl else error "Error: ill-formed interface"
  where
    tl = projTy l p
    tr = projTy r p

recv :: forall a. CVal a => PID -> CPar a
recv p = MRcv p typeRep MRet

trecv :: forall a t. CVal a => t a -> PID -> CPar a
trecv _ p = MRcv p typeRep MRet

run :: (CVal a, CVal b) => a :-> b -> Alg a -> CPar b
run f v = MRun f v MRet

mCse :: (CVal a, CVal b, CVal c)
     => Alg (Either a b)
     -> (Alg a -> CPar c)
     -> (Alg b -> CPar c)
     -> CPar c
mCse (Inl v) f _ = f v
mCse (Inr v) _ g = g v
mCse (Case v l r) f g = mCse v (\x -> mCse (ap l x) f g) (\x -> mCse (ap r x) f g)
mCse mv f g = MCse mv f g (\x -> MRet x)

branch :: CVal a => PID -> CPar a -> CPar a -> CPar a
branch p l r = recv p >>= \(x :: Alg (Either () ())) ->
                            mCse x (\_ -> l) (\_ -> r)

select :: (CVal a, CVal b, CVal c)
       => Alg (Either a b)
       -> [PID]
       -> (Alg a -> CPar c)
       -> (Alg b -> CPar c)
       -> CPar c
select v ps l r = mCse v cl cr
  where
    cl t = bcast (Inl (Lit ()) :: Alg (Either () ())) ps >> l t
    cr t = bcast (Inr (Lit ()) :: Alg (Either () ())) ps >> r t

instance CAp t v => Functor (SPar v t) where
  fmap f (MSnd x p k) = MSnd x p (fmap f k)
  fmap f (MRcv p t k) = MRcv p t $ fmap (fmap f) k
  fmap f (MRun t a k) = MRun t a $ fmap (fmap f) k
  fmap f (MCse e kl kr k) = MCse e kl kr (fmap (fmap f) k)
  fmap f (MIf e kl kr k) = MIf e kl kr (fmap (fmap f) k)
  fmap f (MRet v) = MRet $ f v

instance CAp t v => Applicative (SPar v t) where
  pure = MRet
  af <*> av = join (fmap ((`fmap` af) . flip ($)) av)

join :: SPar v t (SPar v t a) -> SPar v t a
join (MSnd e p k) = MSnd e p $ join k
join (MRcv p t k) = MRcv p t $ fmap join k
join (MRun t a k) = MRun t a $ fmap join k
join (MCse e kl kr k) = MCse e kl kr (fmap join k)
join (MIf e kl kr k) = MIf e kl kr (fmap join k)
join (MRet f) = f

instance CAp t v => Monad (SPar v t) where
  mv >>= kf = join (pure kf <*> mv)

simpl :: CVal a => CPar a -> CPar a
simpl (MSnd e p k) = MSnd e p $ simpl k
simpl (MRcv p t k) = MRcv p t $ \x -> simpl (k x)
simpl (MRun t a k) = MRun t a $ \x -> simpl (k x)
simpl (MCse e kl kr k)
  | isCase (k (BVar 0)) && isInl (kl (BVar 0)) && isInr (kr (BVar 0))
  = mCse e (\x -> simpl $ kl x >>= k) (\x -> simpl $ kr x >>= k)
  | otherwise =
    mCse e (\x -> simpl $ kl x) (\x -> simpl $ kr x) >>= \y -> simpl (k y)
simpl (MIf e kl kr k) = MIf e (simpl kl) (simpl kr) $ \x -> simpl (k x)
simpl m@MRet{} = m

declareParFun :: (CVal a, CVal b) => String -> PID -> (Alg a -> CPar b) -> CGen ASt ()
declareParFun pn p f
  | eraseTy (domTy f) Prelude.== ECUnit = do
      cv <- freshVar
      ctys <- cTySpec cty
      dcv <- cv <:: cty
      body <- codeGen p (f $ CVal $ cVar cUnit) $ cVar cv
      newFun (fn, ctys) []
        (dcv ++ body ++ [CBlockStmt $ CReturn (Just (cVar cv)) undefNode])

  | otherwise = do
      dv <- freshVar
      cv <- freshVar
      ctys <- cTySpec cty
      dtys <- cTySpec dty
      dcv <- cv <:: cty
      body <- codeGen p (f $ CVal $ cVar dv) $ cVar cv
      newFun (fn, ctys) [(dv, dtys)]
        (dcv ++ body ++ [CBlockStmt $ CReturn (Just (cVar cv)) undefNode])
  where
    fn = internalIdent $ pn ++ "_part_" ++ show p
    dty = domTy f
    cty = codTy f

codeGen :: CVal a => PID -> CPar a -> CExpr -> CGen ASt [CBlockItem]
codeGen self = cgen
  where
    cgen :: forall a. CVal a => CPar a -> CExpr -> CGen ASt [CBlockItem]
    cgen (MSnd e@(CVal iv)  p  k) rv = do
      s1 <- csend self p (getTy e) $ iv
      s2 <- cgen k rv
      pure $ s1 ++ s2
    cgen (MSnd e  p  k) rv = do
      v1 <- freshVar
      dv1 <- v1 <:: e
      warn $ "Compiling: " ++ printAlg 0 e
      s1 <- compileAlg e $ cVar v1
      s2 <- csend self p (getTy e) $ cVar v1
      s3 <- cgen k rv
      pure $ dv1 ++ s1 ++ s2 ++ s3
    cgen (MRcv p ty fk) rv = do
      v1 <- freshVar
      dv1 <- v1 <:: ty
      s1 <- crecv self p (getTy ty) $ cVar v1
      s2 <- cgen (fk $ CVal $ cVar v1) rv
      pure $ dv1 ++ s1 ++ s2
    cgen (MRun f e fk) rv = do
      v <- freshVar
      dv <- v <:: (Ap f e)

      warn $ "Compiling: " ++ printAlg 0 (ap f e)
      s1 <- compileAlg (ap f e) $ cVar v
      s2 <- cgen (fk $ CVal $ cVar v) rv
      pure $ dv ++ s1 ++ s2
    cgen (MCse e@(CVal v1) kl kr fk) rv = do
      vl <- freshVar
      vr <- freshVar
      vk <- freshVar
      dvl <- if b then pure []
             else vl <:: domTy kl
      dvr <- if b then pure []
             else vr <:: domTy kr
      dvk <- vk <:: domTy fk
      sl <- cgen (kl $ CVal $ cVar vl) $ cVar vk
      sr <- cgen (kr $ CVal $ cVar vr) $ cVar vk
      sk <- cgen (fk $ CVal $ cVar vk) rv
      pure $ dvl ++ dvr ++ dvk ++ cs vl vr sl sr : sk
        where
          b = getTy e Prelude.== ECEither ECUnit ECUnit
          mMember ce f
            | b = ce
            | otherwise = cMember ce f
          cs vl vr sl sr = CBlockStmt $ cCase (mMember v1 tagFld) cL cR
            where
              cL
                | b = sl
                | otherwise = CBlockStmt (cExpr $ cAssign (cVar vl) untagL) : sl
              cR
                | b = sr
                | otherwise = CBlockStmt (cExpr $ cAssign (cVar vr) untagR) : sr
              untagL
                | b = cVar cUnit
                | otherwise = cMember (cMember v1 valFld) inlFld
              untagR
                | b = cVar cUnit
                | otherwise = cMember (cMember v1 valFld) inrFld
    cgen (MCse e kl kr fk) rv = do
      v1 <- freshVar
      vl <- freshVar
      vr <- freshVar
      vk <- freshVar
      dv1 <- v1 <:: e
      dvl <- vl <:: domTy kl
      dvr <- vr <:: domTy kr
      dvk <- vk <:: domTy fk

      warn $ "Compiling: " ++ printAlg 0 e
      s1 <- compileAlg e $ cVar v1
      sl <- cgen (kl $ CVal $ cVar vl) $ cVar vk
      sr <- cgen (kr $ CVal $ cVar vr) $ cVar vk
      sk <- cgen (fk $ CVal $ cVar vk) rv
      pure $ dv1 ++ dvl ++ dvr ++ dvk ++ s1 ++ cs v1 vl vr sl sr : sk
        where
          b = getTy e Prelude.== ECEither ECUnit ECUnit
          mMember ce f
            | b = ce
            | otherwise = cMember ce f
          cs v1 vl vr sl sr = CBlockStmt $ cCase (mMember (cVar v1) tagFld) cL cR
            where
              cL = CBlockStmt (cExpr $ cAssign (cVar vl) untagL) : sl
              cR = CBlockStmt (cExpr $ cAssign (cVar vr) untagR) : sr
              untagL
                | b = cVar cUnit
                | otherwise = cMember (cMember (cVar v1) valFld) inlFld
              untagR
                | b = cVar cUnit
                | otherwise = cMember (cMember (cVar v1) valFld) inrFld
    cgen (MIf e kl kr fk) rv = do
      (v1, dv1) <- declVar e
      (v2, dv2) <- declVar $ domTy fk

      warn $ "Compiling: " ++ printAlg 0 e
      cb <- compileAlg e v1
      cx <- cgen kl v2
      cy <- cgen kr v2
      ck <- cgen (fk $ CVal v2) rv
      pure $ dv1 ++ dv2 ++ cb ++
        (CBlockStmt $ CIf v1 (CCompound [] cx undefNode)
                      (Just $ CCompound [] cy undefNode) undefNode) : ck
    cgen (MRet (CVal iv)) rv =
      pure [CBlockStmt  $ cExpr $ CAssign CAssignOp rv iv undefNode]
    cgen (MRet v) rv = do
      warn $ "Compiling: " ++ printAlg 0 v
      compileAlg v rv

domTy :: CVal a => (Alg a -> CPar c) -> CTy a
domTy _ = getCTy

codTy :: CVal c => (Alg a -> CPar c) -> CTy c
codTy _ = getCTy
