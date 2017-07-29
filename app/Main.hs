module Main where

import           Vihs
import           System.Environment (getArgs)

main :: IO Vihs.VihsState
main =  do args <- getArgs
           buff <- readFile $ args !! 0
           vihsRun $ vihsInit (args !! 0) $ lines buff
    --ed =<< getArgs
