module Ed where

import           Command
import           Data.Maybe
import           Delete
import           ReadWrite
import           System.Console.Haskeline
import           Text.Parsec

ed :: [String] -> IO ()
ed args =
    if null args
        then (fromMaybe "" <$> runInputT defaultSettings (getInputLine ""))
            >>= (\x ->
                ed'
                    (setCmd x)
                    []
                    []
                    1
                    True)
        else createBuffer (head args)
            >>= (\x ->
                ((fromMaybe "" <$> runInputT defaultSettings (getInputLine ""))
            >>= (\y ->
                ed'
                    (setCmd y)
                    (head args)
                    x
                    1
                    True)))
                where
                    ed' :: Command -> String -> [String] -> Int -> Bool -> IO ()
                    ed' cmd fileName buff crrLine saved
                        | cmdName cmd == 'q' && saved = return ()
                        | cmdName cmd == 'q' && not saved = do
                            putStrLn "?"
                            newCmd <- fromMaybe "" <$> runInputT defaultSettings (getInputLine "")
                            if cmdName (setCmd newCmd) == 'q'
                                then
                                    ed'
                                        (setCmd newCmd)
                                        fileName
                                        buff
                                        crrLine
                                        True
                                else
                                    ed'
                                        (setCmd newCmd)
                                        fileName
                                        buff
                                        crrLine
                                        False
                        | cmdName cmd == 'a' =
                            insert
                            >>= (\x -> (fromMaybe "" <$> runInputT defaultSettings (getInputLine ""))
                            >>= (\y ->
                                ed'
                                    (setCmd y)
                                    fileName
                                    (iCmd
                                        buff
                                        x
                                        $ fromMaybe crrLine (addr1 cmd) + 1)
                                    crrLine
                                    False))
                        | cmdName cmd == 'i' =
                            insert
                            >>= (\x -> (fromMaybe "" <$> runInputT defaultSettings (getInputLine ""))
                            >>= (\y ->
                                ed'
                                    (setCmd y)
                                    fileName
                                    (iCmd
                                        buff
                                        x
                                        $ fromMaybe crrLine $ addr1 cmd)
                                    crrLine
                                    False))
                        | cmdName cmd == 'd' =
                            fromMaybe "" <$> runInputT defaultSettings (getInputLine "")
                            >>= (\x ->
                                ed'
                                    (setCmd x)
                                    fileName
                                    (deleteLine
                                        buff
                                        (fromMaybe crrLine $ addr1 cmd)
                                        (fromMaybe 1 $ addr2 cmd))
                                    crrLine
                                    False)
                        | cmdName cmd == 'l' = do
                            let allLines = map (++"$") buff
                            putStr $ unlines $
                                drop
                                    (fromMaybe crrLine (addr1 cmd) - 1)
                                    (reverse
                                        (drop
                                            (length allLines - (fromMaybe 1 (addr1 cmd) + fromMaybe 1 (addr2 cmd) - 1))
                                            $ reverse allLines))
                            fromMaybe "" <$> runInputT defaultSettings (getInputLine "")
                            >>= (\x ->
                                ed'
                                    (setCmd x)
                                    fileName
                                    buff
                                    crrLine
                                    saved)
                        | cmdName cmd == 'n' = do
                            let infNo = map show (take (length buff) [1, 2..])
                            let allLines = zipWith (++) (map (take 8 . (++ repeat ' ')) infNo) (map (++"$") buff)
                            putStr $ unlines $
                                drop
                                    (fromMaybe crrLine (addr1 cmd) - 1)
                                    (reverse
                                        (drop
                                            (length allLines - (fromMaybe 1 (addr1 cmd) + fromMaybe 1 (addr2 cmd) - 1))
                                            $ reverse allLines))
                            fromMaybe "" <$> runInputT defaultSettings (getInputLine "")
                            >>= (\x ->
                                ed'
                                    (setCmd x)
                                    fileName
                                    buff
                                    crrLine
                                    saved)
                        | cmdName cmd == 'w' =
                            if isNothing $ param cmd
                                then putStrLn "?"
                                    >> fromMaybe "" <$> runInputT defaultSettings (getInputLine "")
                                    >>= (\x ->
                                        ed'
                                            (setCmd x)
                                            fileName
                                            buff
                                            crrLine
                                            saved)
                                else buffToFile (fromJust (param cmd)) buff
                                    >> (print (length (unlines buff))
                                    >> fromMaybe "" <$> runInputT defaultSettings (getInputLine "")
                                    >>= (\x ->
                                        ed'
                                            (setCmd x)
                                            fileName
                                            buff
                                            crrLine
                                            True))
                        | otherwise = do
                            putStrLn "?"
                            fromMaybe "" <$> runInputT defaultSettings (getInputLine "")
                            >>= (\x ->
                                ed'
                                    (setCmd x)
                                    fileName
                                    buff
                                    crrLine
                                    saved)

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
