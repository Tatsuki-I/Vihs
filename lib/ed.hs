import System.Console.Haskeline
import Data.Maybe
import Text.Parsec

ed :: IO ()
ed =
  (fromMaybe "" <$> (runInputT defaultSettings $ getInputLine "")) >>= (\x -> ed' x [] True)
    where
      ed' :: String -> [String] -> Bool -> IO ()
      ed' cmd buff saved
        | cmd == "q" && saved = return ()
        | cmd == "q" && not saved = do
          putStrLn "?"
          newCmd <- fromMaybe "" <$> (runInputT defaultSettings $ getInputLine "")
          if newCmd == "q"
            then ed' newCmd buff True
            else ed' newCmd buff False
        | cmd == "a" =
          insert >>= (\x -> (fromMaybe "" <$> (runInputT defaultSettings $ getInputLine "")) >>= (\y -> ed' y x False))
        | (last cmd) == 'i' =
          insert >>= (\x -> (fromMaybe "" <$> (runInputT defaultSettings $ getInputLine "")) >>= (\y -> ed' y (iCmd buff x (read $ init cmd)) False))
        | (last cmd) == 'd' =
          (fromMaybe "" <$> (runInputT defaultSettings $ getInputLine "")) >>= (\x -> ed' x (deleteLine buff $ read $ init cmd) False)
        | cmd == "l" = do
          putStr $ unlines (map (++"$") buff)
          (fromMaybe "" <$> (runInputT defaultSettings $ getInputLine "")) >>= (\x -> ed' x buff saved)
        | (head cmd) == 'w' = do
          buffToFile ((words cmd) !! 1) buff
          print $ length (unlines buff)
          (fromMaybe "" <$> (runInputT defaultSettings $ getInputLine "")) >>= (\x -> ed' x buff True)
        | otherwise = do
          putStrLn "?"
          (fromMaybe "" <$> (runInputT defaultSettings $ getInputLine "")) >>= (\x -> ed' x buff saved)

buffToFile :: String -> [String] -> IO ()
buffToFile path str = writeFile path $ unlines str

iCmd :: [String] -> [String] -> Int -> [String]
iCmd buff buff2 line =
  (take (line - 1) buff) ++ buff2 ++ (reverse (take ((length buff) - line + 1) (reverse buff)))

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

deleteLine :: [String] -> Int ->[String]
deleteLine str line | line <= length str && line >= 0 && line - 1 <= length str =
  (take (line - 1) str) ++ (reverse . take ((length str) - line) $ reverse str)

main = ed
