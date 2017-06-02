import Text.Parsec

data Command = Command
  { addr1 :: Maybe Int
  , addr2 :: Maybe Int
  , cmd   :: Char
  , param :: Maybe String
  } deriving Show 

setCmd :: String -> Command
setCmd cmdStr = do
  str = words cmdStr
  
  
 
main = do
  print c1
    where
      c1 = Command (Just 1) (Just 4) 'd' Nothing
