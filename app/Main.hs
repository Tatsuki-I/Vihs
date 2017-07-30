module Main where

import Vihs
import Control.Monad

main :: IO ()
main =  Control.Monad.void vihsTestRun
{-
import           System.Environment (getArgs)

main :: IO VihsState
main =  do args <- getArgs
           buff <- readFile $ args !! 0
           vihsRun $ vihsInit (args !! 0) $ lines buff
           -}
