import Data.Either
import Text.Parsec
import Text.Parsec.String

data Command = Command
  { addr1 :: Maybe Int
  , addr2 :: Maybe Int
  , cmd   :: Char
  , param :: Maybe String
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
        addrs = (parseIntList parseText $ init $ (words str) !! 0)

parseInt :: Parser Int
parseInt = do
  value <- many1 digit
  return (read value)

parseText :: Parser [Int]
parseText = parseInt `sepBy1` (char ',')

parseIntList :: Parser [Int] -> String -> [Int]
parseIntList parser input
  = case (parse parser "" input) of
    Left err -> []
    Right x -> x

main = do
  print $ setCmd "2,3d"
  print $ setCmd "w aaa.txt"
  print $ setCmd "1i"
  print $ setCmd "l"
