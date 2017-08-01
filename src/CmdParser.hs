module CmdParser where

import Text.Parsec

import Control.Arrow
import Data.Char
import Text.Read 

test :: Parsec String u String
test =  many (digit  <|> letter)

parseCmd' :: String -> (Maybe Int, String)
parseCmd' =  first readMaybe . span isDigit
