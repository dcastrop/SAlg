{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.CGen
  ( CGen
  , CGSt
  , ECTy(..)
  , PID
  , initCGSt
  , freshVar
  , freshN
  , cVar
  , cInt
  , cFlt
  , cDbl
  , cStr
  , declare
  , newFun
  , newVar
  , varDecl
  , getChan
  , csend
  , crecv
  , cExpr
  , cAssign
  , cCall
  , cMember
  , generateFile
  , isDeclared
  , module X
  ) where

import Control.Monad.RWS
import Control.Monad.Except
import Data.Map.Strict ( Map )
import Data.List ( intersperse )
import qualified Data.Map.Strict as Map
import Language.C.Data as X
import Language.C.Syntax as X
import Language.C.Pretty

import Text.PrettyPrint ( render )

data CGErr

instance Show CGErr where
  show _ = "error"

newtype CGLog = CGLog { unCGLog :: [String] }
instance Show CGLog where
  show = concat . intersperse "\n" . unCGLog

instance Semigroup CGLog where
  l1 <> l2 = CGLog $ unCGLog l1 ++ unCGLog l2

instance Monoid CGLog where
  mempty = CGLog []
  mappend = (<>)

type PID = Integer

data ECTy
  = ECUnit
  | ECInt
  | ECBool
  | ECFlt
  | ECDbl
  | ECStr
  | ECPair ECTy ECTy
  | ECEither ECTy ECTy
  | ECVec ECTy
  deriving (Eq, Ord)

data Chan = Chan { chname :: Ident
                 , chsend :: Ident
                 , chrecv :: Ident
                 }

data CGSt =
  CGSt { varGen :: [String] -- ^ Free variable generator
       , decls :: Map Ident CExtDecl -- ^ Mapping from identifier to declaration
       , declOrder :: [Ident]
       , channels :: Map (PID, PID, ECTy) Chan
       }
--       , decls :: [(String, CDecl)]
--       }

isDeclared :: Ident -> CGen Bool
isDeclared i = Map.member i <$> gets decls

newtype CGen a = CGen { unCGen :: ExceptT CGErr (RWS () CGLog CGSt) a }
  deriving (Functor, Applicative, Monad)

generateFile :: FilePath -> CGen () -> IO ()
generateFile fp m =
  case runRWS (runExceptT $ unCGen m) () initCGSt of
    (Left err, _, clog) -> do
      putStrLn $ show clog
      putStrLn $ "\n"
      putStrLn $ show err
    (_, st, clog) -> do
      putStrLn $ show clog
      writeFile fp $ serialise st ++ "\n"
  where
    serialise st
      = concat $ intersperse "\n\n" $ map lookDef $ reverse $ declOrder st
      where
        lookDef d = render $ pretty $ decls st Map.! d


deriving instance MonadReader () CGen
deriving instance MonadWriter CGLog CGen
deriving instance MonadState CGSt CGen
deriving instance MonadError CGErr CGen

initCGSt :: CGSt
initCGSt = CGSt { varGen = vgen
                , decls = Map.empty
                , declOrder = []
                , channels = Map.empty
                }
  where
    vgen = map ("v_"++) gen
    gen = [[c] | c <- ['a'..'z']]
          ++ [ c ++ show i | c <- gen, i <- [1 :: Integer ..] ]

freshVar :: CGen Ident
freshVar = gets varGen >>= \(h:t) -> do
  ds <- gets decls
  let nh = internalIdent h
  if nh `Map.member` ds
    then modify (\s -> s { varGen = t }) *> freshVar
    else pure (internalIdent h) <* modify (\s -> s { varGen = t} )

freshN :: String -> CGen Ident
freshN f = go (Nothing :: Maybe Int) <$> gets decls
  where
    fullName Nothing  = f
    fullName (Just i) = f ++ "_" ++ show i
    go m ds =
      let nh = internalIdent $ fullName m in
      if nh `Map.member` ds
        then go (maybe (Just 0) (\i -> Just (i+1)) m) ds
        else nh

cVar :: Ident -> CExpr
cVar s = CVar s undefNode

cInt :: Int -> CExpr
cInt i = CConst $ CIntConst (cInteger $ fromIntegral i) undefNode

cFlt :: Float -> CExpr
cFlt i = CConst $ CFloatConst (cFloat i) undefNode

cDbl :: Double -> CExpr
cDbl i = CConst $ CFloatConst (cFloat $ realToFrac i) undefNode

cStr :: String -> CExpr
cStr s = CConst $ CStrConst (cString s) undefNode

declare :: Ident -> CTypeSpec -> CGen CTypeSpec
declare nm cd = do
  s <- get
  if Map.member nm $ decls s
    then pure $ CTypeDef nm undefNode
    else do put s { decls = Map.insert nm (CDeclExt dd) $ decls s
                  , declOrder = nm : declOrder s
                  }
            pure $ CTypeDef nm undefNode
  where
    dd = CDecl [CStorageSpec (CTypedef undefNode), CTypeSpec cd]
         [(Just (CDeclr (Just nm) [] Nothing [] undefNode), Nothing, Nothing)]
         undefNode

newFun :: (Ident, (CTypeSpec, [CDerivedDeclr]))
       -> (Ident, (CTypeSpec, [CDerivedDeclr]))
       -> [CBlockItem]
       -> CGen ()
newFun (f, (rty, fq)) (x, (xty, xq)) body =
  modify $ \s -> s { decls = Map.insert f (CFDefExt fdef) $ decls s
                   , declOrder = f : declOrder s
                   }
  where
    fdef = CFunDef [CTypeSpec rty] fdeclr [] cbody undefNode
    fdeclr = CDeclr (Just f) (fundeclr : fq) Nothing [] undefNode
    cbody = CCompound [] body undefNode
    fundeclr = CFunDeclr (Right ([arg], False)) [] undefNode
    arg = CDecl [CTypeSpec xty] [(Just xdeclr, Nothing, Nothing)] undefNode
    xdeclr = CDeclr (Just x) xq Nothing [] undefNode


newVar :: CTypeSpec -> [CDerivedDeclr] -> Maybe CInit -> CGen Ident
newVar t q i = do
  v <- freshVar
  modify $ \s ->
    s { decls = Map.insert v (CDeclExt $ varDecl v t q i) $ decls s
      , declOrder = v : declOrder s
      }
  pure v

varDecl :: Ident -> CTypeSpec -> [CDerivedDeclr] -> Maybe CInit -> CDecl
varDecl v t q i = CDecl [CTypeSpec t] [(Just dc, i, Nothing)] undefNode
  where
    dc = CDeclr (Just v) q Nothing [] undefNode

getChan :: PID -> PID -> ECTy -> CGen Chan
getChan from to ty = do
  s <- get
  case Map.lookup (from, to, ty) (channels s) of
    Just i -> pure i
    Nothing -> do
      let sz = show $ Map.size (channels s)
          chn = internalIdent $ "ch" ++ sz
          sendc = internalIdent $ "send_ch" ++ sz
          recvc = internalIdent $ "recv_ch" ++ sz
          ch = Chan { chname = chn, chsend = sendc, chrecv = recvc }
      put s { channels = Map.insert (from, to, ty) ch $ channels s }
      pure ch

csend :: PID -> PID -> ECTy -> CExpr -> CGen [CBlockItem]
csend from to ty v = do
  c <- getChan from to ty
  pure $ [CBlockStmt $ cExpr $
    CCall (cVar $ chsend c) [cVar $ chname c, v] undefNode]

cAddr :: CExpr -> CExpr
cAddr e = CUnary CAdrOp e undefNode

crecv :: PID -> PID -> ECTy -> CExpr -> CGen [CBlockItem]
crecv from to ty v = do
  c <- getChan to from ty
  pure $ [ CBlockStmt $ cExpr $
           CCall (cVar $ chrecv c) [cVar $ chname c, cAddr v] undefNode ]

cExpr :: CExpr -> CStat
cExpr e = CExpr (Just e) undefNode

cAssign :: Ident -> CExpr -> CExpr
cAssign vv e = CAssign CAssignOp (cVar vv) e undefNode

cCall :: Ident -> [CExpr] -> CExpr
cCall fn e = CCall (cVar fn) e undefNode

cMember :: CExpr -> Ident -> CExpr
cMember e i = CMember e i False undefNode
