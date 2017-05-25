createBuffer :: String -> IO [String]
createBuffer path = do
    file <- readFile path
    return $ lines file

main = do
    str <- createBuffer "test.txt"
    print str
