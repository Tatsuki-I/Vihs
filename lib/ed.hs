import Text.Parsec

ed :: IO [String]
ed = do
  putStr "? "
  cmd <- getLine
  ed' cmd []
  where
    ed' :: String -> [String] -> IO [String]
    ed' cmd buff
      | cmd == "q" = return buff
      | cmd == "a" = do
        buff <- insert
        print buff
        putStr "? "
        cmd <- getLine
        ed' cmd buff
      | (last cmd) == 'i' = do
        buff2 <- insert
        print $ init cmd
        let buff3 = iCmd buff buff2 (read $ init cmd)
        print buff3
        putStr "? "
        cmd <- getLine
        ed' cmd buff3
      | cmd == "l" = do
        putStr $ unlines buff
        putStr "? "
        cmd <- getLine
        ed' cmd buff
      | (head cmd) == 'w' = do
        print $ (words cmd) !! 1
        buffToFile ((words cmd) !! 1) buff
        putStr "? "
        cmd <- getLine
        ed' cmd buff
      | otherwise = do
        putStrLn $ "Vihs: command not found"
        putStr "? "
        cmd <- getLine
        ed' cmd buff

buffToFile :: String -> [String] -> IO ()
buffToFile path str = writeFile path $ unlines str

iCmd :: [String] -> [String] -> Int -> [String]
iCmd buff buff2 line = (take (line - 1) buff) ++ buff2 ++ (reverse (take ((length buff) - line + 1) (reverse buff)))

insert :: IO [String]
insert = insert' [] False
  where
    insert' :: [String] -> Bool -> IO [String]
    insert' buff done
      | done = return buff
      | otherwise = do
        str <- getLine
        if str == "."
          then insert' buff True
          else insert' (buff ++ [str]) False

main = ed
