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
  (cmdName)
  (if (length (parseByCmd str)) < 1
    then Nothing
    else Just param)
      where
        addrs = (parseIntList ((parseByCmd str) !! 0))
        param = ((parseByCmd str) !! 1)
        cmdName = []


parseInt :: Parser Int
parseInt = do
  value <- many1 digit
  return (read value)

parseText :: Parser [Int]
parseText = parseInt `sepBy1` (char ',')


parseString :: Parser String
parseString = do
        value <- string
        return value

parseByCmd :: Parser [String]
parseByCmd = parseString `sepBy1` (letter)

parseStrList :: String -> [String]
parseStrList input
  = case (parse parseByCmd "" input) of
    Left err -> []
    Right x -> x

parseIntList :: String -> [Int]
parseIntList input
  = case (parse parseText "" input) of
    Left err -> []
    Right x -> x


main = do
        print $ setCmd "w test.txt"
