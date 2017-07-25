module ParseCmd where

import           Control.Applicative
import qualified Text.Parsec         as P
import           Text.Parsec.String

data Command = Command { addr    :: Maybe Addr
                       , cmdName :: Char
                       , param   :: Maybe String
                       } deriving (Show, Eq)

data Addr = AddrSingle AddrVal
          | AddrPair  AddrVal AddrVal
          deriving (Show, Eq)

data AddrVal = AddrLine Int
             | AddrCrr Int
             | AddrEOF
             deriving (Show, Eq)

addrCrrLine :: AddrVal
addrCrrLine =  AddrCrr 0

setCmd     :: String -> Command
setCmd str =  case P.parse parseCmd "" str of
                Right cmd -> cmd
                Left  err -> Command Nothing ' ' Nothing
                --putStrLn ("No match: " ++ show err)

parseCmd :: Parser Command
parseCmd =  Command <$> parseAddr
                    <*> parseCmdName
                    <*> parseParam

parseAddr :: Parser (Maybe Addr)
parseAddr =  optional parseAddr'
  where
    parseAddr' = parseHeadToEOF
             <|> parseCrrToEOF 
             <|> parseAddrs

    parseHeadToEOF = P.char ','
                  *> pure (AddrPair (AddrLine 1) AddrEOF)

    parseCrrToEOF = P.char ';'
                 *> pure (AddrPair addrCrrLine AddrEOF)

    parseAddrs = do (x:xs) <- parseIntList
                    return $ if null xs
                               then AddrSingle $ AddrLine x
                               else AddrPair (AddrLine x) (AddrLine $ last xs)

parseCmdName :: Parser Char
parseCmdName =  P.letter

parseParam :: Parser (Maybe String)
parseParam =  P.spaces
          *> (listToMaybeList <$> P.many P.anyChar)
            where
              listToMaybeList [] = Nothing
              listToMaybeList xs = Just xs

parseIntList :: Parser [Int]
parseIntList =  parseInt `P.sepBy1` P.char ','

parseInt :: Parser Int
parseInt =  do value <- P.many1 P.digit
               return (read value)
