module Ed where

import System.Console.Haskeline
import Data.Maybe
import Text.Parsec
import Command

ed :: IO ()
ed =
  (fromMaybe "" <$> (runInputT defaultSettings $ getInputLine "")) >>= (\x -> ed' (setCmd x) [] 1 True)
    where
      ed' :: Command -> [String] -> Int -> Bool -> IO ()
      ed' cmd buff crrLine saved
        | (cmdName cmd) == 'q' && saved = return ()
        | (cmdName cmd) == 'q' && not saved = do
          putStrLn "?"
          newCmd <- fromMaybe "" <$> (runInputT defaultSettings $ getInputLine "")
          if (cmdName (setCmd newCmd)) == 'q'
            then ed' (setCmd newCmd) buff crrLine True
            else ed' (setCmd newCmd) buff crrLine False
        | (cmdName cmd) == 'a' =
          insert >>= (\x -> (fromMaybe "" <$> (runInputT defaultSettings $ getInputLine "")) >>= (\y -> ed' (setCmd y) (iCmd buff x $ (fromMaybe crrLine $ addr1 cmd) + 1) crrLine False))
        | (cmdName cmd) == 'i' =
          insert >>= (\x -> (fromMaybe "" <$> (runInputT defaultSettings $ getInputLine "")) >>= (\y -> ed' (setCmd y) (iCmd buff x $ fromMaybe crrLine $ addr1 cmd) crrLine False))
        | (cmdName cmd) == 'd' =
          (fromMaybe "" <$> (runInputT defaultSettings $ getInputLine "")) >>= (\x -> ed' (setCmd x) (deleteLine buff (fromMaybe crrLine $ addr1 cmd) (fromMaybe 1 $ addr2 cmd)) crrLine False)
        | (cmdName cmd) == 'l' = do
          let allLines = (map (++"$") buff)
          putStr $
            unlines $ drop
              ((fromMaybe crrLine $ addr1 cmd) - 1)
              (reverse (drop
                ((length allLines) - ((fromMaybe 1 $ addr1 cmd) + (fromMaybe 1 $ addr2 cmd) - 1))
                $ reverse allLines))
          (fromMaybe "" <$> (runInputT defaultSettings $ getInputLine "")) >>= (\x -> ed' (setCmd x) buff crrLine saved)
        | (cmdName cmd) == 'n' = do
          let infNo = (map (show) (take (length buff) [1, 2..]))
          let allLines = (zipWith (++) (map (take 8) ((map (++"        ")) infNo)) (map (++"$") buff))
          putStr $
            unlines $ drop
              ((fromMaybe crrLine $ addr1 cmd) - 1)
              (reverse (drop
                ((length allLines) - ((fromMaybe 1 $ addr1 cmd) + (fromMaybe 1 $ addr2 cmd) - 1))
                $ reverse allLines))
          (fromMaybe "" <$> (runInputT defaultSettings $ getInputLine "")) >>= (\x -> ed' (setCmd x) buff crrLine saved)
        | (cmdName cmd) == 'w' = do
          if (isNothing $ param cmd)
            then putStrLn "?" >> (fromMaybe "" <$> (runInputT defaultSettings $ getInputLine "")) >>= (\x -> ed' (setCmd x) buff crrLine saved)
            else (buffToFile (fromJust (param cmd)) buff) >> (print (length (unlines buff)) >> (fromMaybe "" <$> (runInputT defaultSettings $ getInputLine "")) >>= (\x -> ed' (setCmd x) buff crrLine True))
        | otherwise = do
          putStrLn "?"
          (fromMaybe "" <$> (runInputT defaultSettings $ getInputLine "")) >>= (\x -> ed' (setCmd x) buff crrLine saved)

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

deleteLine :: [String] -> Int -> Int ->[String]
deleteLine str line times
  | line <= length str && line >= 0 && line - 1 <= length str =
    (take (line - 1) str) ++ (reverse . take ((length str) - line - times + 1) $ reverse str)
