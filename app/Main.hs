module Main where

import           Ed
import           System.Environment (getArgs)

main :: IO ()
main = ed =<< getArgs
