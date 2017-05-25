module ReadWrite where

createBuffer :: String -> IO [String]
createBuffer path = do
    file <- readFile path
    return $ lines file

buffToFile :: String -> [String] -> IO ()
buffToFile path str = do
    writeFile path $ unlines str
