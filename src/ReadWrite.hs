module ReadWrite where

createBuffer :: String -> IO [String]
createBuffer path = readFile path >>= \x -> return $ lines x

buffToFile :: String -> [String] -> IO ()
buffToFile path str = writeFile path $ unlines str
