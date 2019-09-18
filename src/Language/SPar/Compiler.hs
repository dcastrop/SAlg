{-# LANGUAGE TupleSections #-}
module Language.SPar.Compiler
  ( compile
  ) where

import Data.List ( intersperse, foldl' )
import Language.Haskell.Interpreter
import System.FilePath
-- import System.Exit

compile :: FilePath -> [String] -> IO ()
compile fp _args = do
  r <- runInterpreter loadPar
  case r of
    Left err ->
      case err of
        UnknownError s -> putStrLn $ "Unknown error: " ++ s
        WontCompile es -> putStrLn $ "Compilation error: " ++
                          concat (intersperse "\n" $ map errMsg es)
        NotAllowed s -> putStrLn $ "Not allowed: " ++ s
        GhcException s -> putStrLn $ "Exception: " ++ s

    Right () -> pure ()
  where
    loadPar :: InterpreterT IO ()
    loadPar = do
      loadModules [fp]
      setTopLevelModules [takeBaseName fp]

      setImports [ "System.Environment"
                 , "Language.SPar.Skel"
                 , "Control.Monad.CGen"
                 , "Control.Monad"
                 , "Prelude"
                 ]
      fns <- getModuleExports $ takeBaseName fp
      ftys <- (map fst . filter snd) <$>
        mapM (\e ->
                case e of
                  Fun nm -> (nm,) <$> typeChecks (compileCommand nm)
                  _ -> pure ("", False)
             ) fns

      liftIO $ mapM_ (\a -> putStrLn $ "Compiling '" ++ a ++ "'") ftys
      compileAll $ ftys

    compileAll :: [String] -> InterpreterT IO ()
    compileAll fns = liftIO (putStrLn command) >> runStmt command
      where
        command = "withProgName " ++ show (takeBaseName fp)
          ++ " (generateFile \"" ++ (takeBaseName fp ++ "\" $ ")
          ++ foldl' (\a b -> a ++ " >> " ++ compileCommand b) "pure ()" fns
          ++ ")"

compileCommand :: [Char] -> [Char]
compileCommand nm = "compileAsLib "++ show nm ++ " mempty " ++ nm
