module ReadWrite where

createBuffer :: String -> IO [String]
createBuffer path = do
    file <- readFile path
    return $ lines file
