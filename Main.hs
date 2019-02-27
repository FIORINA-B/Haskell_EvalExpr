module Main where

import Calc
import Text.Printf
import System.Environment

main = do (x:_) <- getArgs;
          case evalExpr x of
            Nothing -> error "Failed"
            Just ret -> printf "%.2f%s" ret "\n"
