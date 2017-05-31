import Text.Parsec

{-
cmd :: String -> IO Int
cmd cmdstr = do
    num <- parseTest numbers cmdstr
    return num
-}

numbers = many1 digit >>= \x -> return (read x :: Int)

{-
cmd = many1 letter >>= \x -> return (read x :: String)
-}

commandWithCount = do
    count <- numbers
    command <- (many1 letter)
    return (count, command)

main = do
    parseTest commandWithCount "1dd"
