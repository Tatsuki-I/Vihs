import           Data.Either
import           Text.Parsec
import           Text.Parsec.String

data Command
        = Command
        { addr    :: Maybe Addr
        , cmdName :: Char
        , param   :: Maybe String
        } deriving (Show, Eq)

data Addr
        = AddrSingle AddrVal
        | AddrPair  AddrVal AddrVal
        deriving (Show, Eq)

data AddrVal
        = AddrLine Int
        | AddrCrr Int
        | AddrEOF
        deriving (Show, Eq)

addrCrrLine :: AddrVal
addrCrrLine = AddrCrr 0

parseCmd :: Parser Command
parseCmd = Command
        <$> parseAddr
        <*> parseCmdName
        <*> parseParam

parseAddr :: Parser (Maybe Addr)
parseAddr = optional parseAddr'
        where
                parseAddr'
                        =   parseHeadToEOF
                        <|> parseCrrToEOF
                        <|> parsePair
                        <|> parseSingle

                parseHeadToEOF = char ','
                        *> pure $ AddrPair AddrLine 1 AddrEOF

                parseCrrToEOF = char ';'
                        *> pure $ AddrPair AddrCrrLine AddrEOF

                parsePair = AddrPair
                        <$> head parseIntList
                        <$> last parseIntList

                parseSingle = AddrSingle
                        <$> head parseIntList

parseCmdName :: Parser Char
parseCmdName = letter

parseParam :: Parser (Maybe String)
parseParam = spaces
        *> (listToMaybeList <$> many anyChar)
                where
                        listToMaybeList [] = Nothing
                        listToMaybeList xs = Just xs


parseIntList :: String -> [Int]
parseIntList input
  = case parse parseText "" input of
    Left err -> []
    Right x  -> x

parseText :: Parser [Int]
parseText = parseInt `sepBy1` char ','

parseInt :: Parser Int
parseInt = do
        value <- many1 digit
        return (read value)

main = do
        print $ parse parseCmd "" "w test.txt"
        print $ parse parseCmd "" "1,2a"
        print $ parse parseCmd "" ",d"

