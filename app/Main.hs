module Main where

import Language.SPar.Compiler
import System.Environment
import System.Exit

main :: IO ()
main = getArgs >>= \a ->
  case a of
    [] -> die "Error: no input file"
    (fp:args) -> compile fp args
