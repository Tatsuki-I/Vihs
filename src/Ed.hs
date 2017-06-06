module Ed where

import           Command
import           Data.Maybe
import           Delete
import           ReadWrite
import           System.Console.Haskeline
import           Text.Parsec

ed :: [String] -> IO ()
ed args = do
    x <- if null args then return [] else createBuffer (head args)
    y <- inputCmd
    ed' y (if null args then [] else (head args)) x 1 True
        where
            ed' :: Command -> String -> [String] -> Int -> Bool -> IO ()
            ed' cmd fileName buff crrLine saved  = case cmdName cmd of
                'q' -> 
                    if saved then return () 
                    else do
                        putStrLn "?"
                        newCmd <- inputCmd
                        ed' newCmd fileName buff crrLine $ if cmdName newCmd == 'q' then True else False
                'a' ->
                    insert >>= (\x -> inputCmd >>= (\y ->
                    ed' y fileName (iCmd buff x $ fromMaybe crrLine (addr1 cmd) + 1) crrLine False))
                'i' ->
                    insert >>= (\x -> inputCmd >>= (\y ->
                    ed' y fileName (iCmd buff x $ fromMaybe crrLine $ addr1 cmd) crrLine False))
                'd' ->
                    inputCmd >>= (\x ->
                    ed' x fileName (deleteLine buff (fromMaybe crrLine $ addr1 cmd) (fromMaybe 1 $ addr2 cmd)) crrLine False)
                'l' -> do
                    let allLines = addDll buff
                    putStr $ unlines $
                        drop
                            (fromMaybe crrLine (addr1 cmd) - 1)
                            (reverse
                                (drop
                                    (length allLines - (fromMaybe 1 (addr1 cmd) + fromMaybe 1 (addr2 cmd) - 1))
                                    $ reverse allLines))
                    inputCmd >>= (\x -> ed' x fileName buff crrLine saved)
                'n' -> do
                    let infNo = map show (take (length buff) [1, 2..])
                    let allLines = zipWith (++) (map (take 8 . (++ repeat ' ')) infNo) (addDll buff)
                    putStr $ unlines $
                        drop
                            (fromMaybe crrLine (addr1 cmd) - 1)
                            (reverse
                                (drop
                                    (length allLines - (fromMaybe 1 (addr1 cmd) + fromMaybe 1 (addr2 cmd) - 1))
                                    $ reverse allLines))
                    inputCmd >>= (\x -> ed' x fileName buff crrLine saved)
                'w' ->
                    if isNothing $ param cmd
                        then putStrLn "?"
                            >> inputCmd >>= (\x -> ed' x fileName buff crrLine saved)
                        else buffToFile (fromJust (param cmd)) buff
                            >> (print (length (unlines buff))
                            >> inputCmd >>= (\x -> ed' x fileName buff crrLine True))
                otherwise -> do
                    putStrLn "?"
                    inputCmd >>= (\x -> ed' x fileName buff crrLine saved)

inputCmd :: IO Command
inputCmd = setCmd <$> fromMaybe "" <$> runInputT defaultSettings (getInputLine "")

addDll :: [String] -> [String]
addDll buff = map (++"$") buff

iCmd :: [String] -> [String] -> Int -> [String]
iCmd buff buff2 line =
    take (line - 1) buff ++ buff2 ++ reverse (take (length buff - line + 1) (reverse buff))

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
