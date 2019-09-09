{-# LANGUAGE IncoherentInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.C
  ( CVal (..)
  , CTy(..)
  , getCTyR
  , getTy
  , eitherTy
  , pairTy
  , cTySpec
  , (<::)
  , tagFld
  , valFld
  , inlFld
  , inrFld
  , fstFld
  , sndFld
  , cCase
  , cTagl
  , cTagr
  ) where

import Control.Monad ( join, void )
import Control.Monad.CGen
import Type.Reflection ( Typeable, TypeRep, typeRep )

data CTy a where
  CUnit   :: CTy ()
  CInt    :: CTy Int
  CBool   :: CTy Bool
  CFlt    :: CTy Float
  CDbl    :: CTy Double
  CStr    :: CTy String
  CPair   :: (CVal a, CVal b) => CTy a -> CTy b -> CTy (a,b)
  CEither :: (CVal a, CVal b) => CTy a -> CTy b -> CTy (Either a b)
  CVec    :: CVal a => CTy a -> CTy [a]

eraseTy :: CTy a -> ECTy
eraseTy CUnit         = ECUnit
eraseTy CInt          = ECInt
eraseTy CBool         = ECBool
eraseTy CFlt          = ECFlt
eraseTy CDbl          = ECDbl
eraseTy CStr          = ECStr
eraseTy (CPair   l r) = ECPair (eraseTy l) (eraseTy r)
eraseTy (CEither l r) = ECEither (eraseTy l) (eraseTy r)
eraseTy (CVec    l  ) = ECVec (eraseTy l)

getTy :: forall a t. CVal a => t a -> ECTy
getTy _ = eraseTy (getCTy :: CTy a)

getCTyR :: CTy a -> TypeRep a
getCTyR (CPair _ _) = typeRep
getCTyR (CEither _ _) = typeRep
getCTyR (CVec _) = typeRep
getCTyR CUnit = typeRep
getCTyR CInt = typeRep
getCTyR CBool = typeRep
getCTyR CFlt = typeRep
getCTyR CDbl = typeRep
getCTyR CStr = typeRep

eitherTy :: (CVal a, CVal b) => t a -> t b -> TypeRep (Either a b)
eitherTy _ _ = typeRep

pairTy :: (CVal a, CVal b) => t a -> t b -> TypeRep (a, b)
pairTy _ _ = typeRep

tUnit :: Ident
tUnit = internalIdent "unit"

cUnit :: Ident
cUnit = internalIdent "Unit"

tyUnit :: Ident
tyUnit = internalIdent "unit_t"

unitTySpec :: CTypeSpec
unitTySpec = CEnumType (CEnum (Just tUnit) (Just [(cUnit, Nothing)])
                         [] undefNode) undefNode

cTyName :: String -> CTy a -> Ident
cTyName suff = internalIdent . (++ suff) . tyNm
  where
    tyNm :: CTy a -> String
    tyNm CUnit = "unit"
    tyNm CInt = "int"
    tyNm CBool = "int"
    tyNm CFlt = "float"
    tyNm CDbl = "double"
    tyNm CStr = "string"
    tyNm (CPair l r) = "pair_" ++ tyNm l ++ "_" ++ tyNm r
    tyNm (CEither l r) = "either_" ++ tyNm l ++ "_" ++ tyNm r
    tyNm (CVec a) = "vec_" ++ tyNm a

pairTySpec :: Ident
           -> (CTypeSpec, [CDerivedDeclr])
           -> (CTypeSpec, [CDerivedDeclr])
           -> CTypeSpec
pairTySpec nm (tl, ql) (tr, qr) =
  CSUType (CStruct CStructTag (Just nm) (Just [p1, p2]) [] undefNode) undefNode
  where
    p1 = fld fstFld tl ql
    p2 = fld sndFld tr qr

fld :: Ident -> CTypeSpec -> [CDerivedDeclr] -> CDecl
fld s t q = CDecl [CTypeSpec t] [(fldD, Nothing, Nothing)] undefNode
  where
    fldD = Just $ CDeclr (Just s) q Nothing [] undefNode

tagFld :: Ident
tagFld = internalIdent "tag"

valFld :: Ident
valFld = internalIdent "val"

inlFld :: Ident
inlFld = internalIdent "inl"

inrFld :: Ident
inrFld = internalIdent "inr"

fstFld :: Ident
fstFld = internalIdent "fst"

sndFld :: Ident
sndFld = internalIdent "snd"

tyTag :: Ident
tyTag = internalIdent "tag_t"

tTag :: Ident
tTag = internalIdent "tag"

cTagl :: Ident
cTagl = internalIdent "Inl"

cTagr :: Ident
cTagr = internalIdent "Inr"

tagTySpec :: CTypeSpec
tagTySpec = CEnumType (CEnum (Just tTag) cs [] undefNode) undefNode
  where
    cs = Just [(cTagl, Nothing),(cTagr, Nothing)]

eitherTySpec :: Ident
             -> (CTypeSpec, [CDerivedDeclr])
             -> (CTypeSpec, [CDerivedDeclr])
             -> CTypeSpec
eitherTySpec nm (tl, ql) (tr, qr) =
  CSUType (CStruct CStructTag (Just nm) (Just [p1, p2]) [] undefNode) undefNode
  where
    p1 = fld tagFld (CTypeDef tyTag undefNode) []
    p2 = fld valFld cunion []
    cunion = CSUType csu undefNode
    csu = CStruct CUnionTag Nothing (Just [inj1, inj2]) [] undefNode
    inj1 = fld inlFld tl ql
    inj2 = fld inrFld tr qr

cTySpec :: CTy a -> CGen (CTypeSpec, [CDerivedDeclr])
cTySpec CUnit = (,) <$> declare tyUnit unitTySpec <*> pure []
cTySpec CInt = pure $ (CIntType undefNode, [])
cTySpec CBool = pure $ (CIntType undefNode, [])
cTySpec CFlt = pure $ (CFloatType undefNode, [])
cTySpec CDbl = pure $ (CDoubleType undefNode, [])
cTySpec CStr = pure $ (CCharType undefNode, [CPtrDeclr [] undefNode])
cTySpec p@(CPair l r) = join $ doPair <$> cTySpec l <*> cTySpec r
  where
    doPair cl cr = (,) <$> declare nmt (pairTySpec nm cl cr) <*> pure []
    nmt = cTyName "_t" p
    nm = cTyName "" p
cTySpec p@(CEither l r) = do
  void $ declare tyTag tagTySpec
  join $ doEither <$> cTySpec l <*> cTySpec r
  where
    doEither cl cr
      = (,) <$> declare nmt (eitherTySpec nm cl cr) <*> pure []
    nmt = cTyName "_t" p
    nm = cTyName "" p
cTySpec (CVec a) = addPtr <$> cTySpec a
  where
    addPtr (t, d) = (t, CPtrDeclr [] undefNode : d)

class (Eq a, Ord a, Show a, Typeable a) => CVal a where
  getCTy  :: CTy a
  cVal    :: a -> CGen CExpr

instance CVal () where
  getCTy = CUnit
  cVal () = cTySpec CUnit *> pure (cVar cUnit)

instance CVal Int where
  getCTy = CInt
  cVal i = pure $ cInt i

instance CVal Bool where
  getCTy = CBool
  cVal True = pure $ cInt 1
  cVal False = pure $ cInt 0

instance CVal Float where
  getCTy = CFlt
  cVal i = pure $ cFlt i

instance CVal Double where
  getCTy = CDbl
  cVal i = pure $ cDbl i

instance (CVal a, CVal b) => CVal (a, b) where
  getCTy = CPair getCTy getCTy
  cVal (x, y) = do
    vx <- cVal x
    vy <- cVal y
    (t, q) <- cTySpec $ getCTy @(a,b)
    cVar <$> newVar t q (Just $ ini [mkInit vx, mkInit vy])
      where
        mkInit e = ([], CInitExpr e undefNode)

instance (CVal a, CVal b) => CVal (Either a b) where
  getCTy = CEither getCTy getCTy
  cVal (Left x) = do
    vx <- cVal x
    (t, q) <- cTySpec $ getCTy @(Either a b)
    cVar <$> newVar t q (Just $ ini [tagL, ([], ini [([mkInL], iniE vx)])])
      where
        tagL = ([], iniE $ cVar cTagl)
        mkInL = CMemberDesig (internalIdent "inl") undefNode
  cVal (Right x) = do
    vx <- cVal x
    (t, q) <- cTySpec $ getCTy @(Either a b)
    cVar <$> newVar t q (Just $ ini [tagR, ([], ini [([mkInR], iniE vx)])])
      where
        tagR = ([], iniE $ cVar cTagr)
        mkInR = CMemberDesig (internalIdent "inr") undefNode

ini :: CInitializerList NodeInfo -> CInitializer NodeInfo
ini = (`CInitList` undefNode)
iniE :: CExpression NodeInfo -> CInitializer NodeInfo
iniE = (`CInitExpr` undefNode)

instance (CVal a) => CVal [a] where
  getCTy = CVec getCTy
  cVal _xs = undefined

instance {-# OVERLAPPING #-} CVal String where
  getCTy = CStr
  cVal s = pure $ cStr s

(<::) :: forall a t. CVal a => Ident -> t a -> CGen [CBlockItem]
v <:: _ = do
  (t, q) <- cTySpec (getCTy :: CTy a)
  pure $ [CBlockDecl $ varDecl v t q Nothing]

cCase :: CExpr -> [CBlockItem] -> [CBlockItem] -> CStat
cCase e l r = CSwitch e cases undefNode
  where
    cases = CCompound [] (zipWith mkCase [l, r] [cTagl, cTagr]) undefNode
    mkCase ss tag = CBlockStmt $ CCase (cVar tag) (caseCode ss) undefNode
    caseCode ss = CCompound [] (ss ++ [CBlockStmt $ CBreak undefNode]) undefNode
