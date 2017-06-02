module Command where

import Data.Either
import Text.Parsec
import Text.Parsec.String

data Command = Command
  { addr1   :: Maybe Int
  , addr2   :: Maybe Int
  , cmdName :: Char
  , param   :: Maybe String
  } deriving Show 

setCmd :: String -> Command
setCmd str = Command
  (if addrs == []
    then Nothing
    else Just (addrs !! 0))
  (if length addrs < 2
    then Nothing
    else Just (addrs !! 1))
  (last $ (words str) !! 0)
  (if length (words str) < 2
    then Nothing
    else (Just $ (words str) !! 1))
      where
        addrs = (parseIntList $ init $ (words str) !! 0)

parseInt :: Parser Int
parseInt = do
  value <- many1 digit
  return (read value)

parseText :: Parser [Int]
parseText = parseInt `sepBy1` (char ',')

parseIntList :: String -> [Int]
parseIntList input
  = case (parse parseText "" input) of
    Left err -> []
    Right x -> x
