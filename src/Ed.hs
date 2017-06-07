module Ed where

import           Command
import           Data.Maybe
import           Delete
import           ReadWrite
import           System.Console.Haskeline
import           Text.Parsec

data EdArgs = EdArgs
        { fileName :: String
        , buff :: [String]
        , crrLine :: Int
        , saved :: Bool} deriving Show

ed :: [String] -> IO ()
ed args = do
    x <- if null args then return [] else createBuffer (head args)
    y <- inputCmd
    ed' y (EdArgs (if null args then [] else (head args)) x 1 True)
        where
            ed' :: Command -> EdArgs -> IO ()
            ed' cmd edArgs = case cmdName cmd of
                'q' -> 
                    if saved edArgs then return () 
                    else do
                        putStrLn "?"
                        newCmd <- inputCmd
                        ed' newCmd edArgs {saved = (if cmdName newCmd == 'q' then True else False)}
                'a' ->
                    insert >>= (\x -> inputCmd >>= (\y ->
                    ed' y edArgs {buff = (iCmd (buff edArgs) x $ fromMaybe (crrLine edArgs) (addr1 cmd) + 1), saved = False}))
                'i' ->
                    insert >>= (\x -> inputCmd >>= (\y ->
                    ed' y edArgs {buff = (iCmd (buff edArgs) x $ fromMaybe (crrLine edArgs) $ addr1 cmd), saved = False}))
                'd' ->
                    inputCmd >>= (\x ->
                    ed' x edArgs {buff = (deleteLine (buff edArgs) (fromMaybe (crrLine edArgs) $ addr1 cmd) (fromMaybe 1 $ addr2 cmd)), saved = False})
                'l' -> do
                    let allLines = addDll $ buff edArgs
                    printBuff cmd edArgs allLines
                    inputCmd >>= (\x -> ed' x edArgs)
                'n' -> do
                    let infNo = map show (take (length $ buff edArgs) [1, 2..])
                    let allLines = zipWith (++) (map (take 8 . (++ repeat ' ')) infNo) (addDll $ buff edArgs)
                    printBuff cmd edArgs allLines
                    inputCmd >>= (\x -> ed' x edArgs)
                'w' ->
                    if isNothing $ param cmd
                        then putStrLn "?"
                            >> inputCmd >>= (\x -> ed' x edArgs)
                        else buffToFile (fromJust (param cmd)) (buff edArgs)
                            >> (print (length (unlines $ buff edArgs))
                            >> inputCmd >>= (\x -> ed' x edArgs {saved = True}))
                otherwise ->
                    putStrLn "?" >> inputCmd >>= (\x -> ed' x edArgs)

inputCmd :: IO Command
inputCmd = setCmd <$> fromMaybe "" <$> runInputT defaultSettings (getInputLine "")

addDll :: [String] -> [String]
addDll buff = map (++"$") buff

printBuff :: Command -> EdArgs -> [String] -> IO ()
printBuff cmd edArgs allLines = 
        putStr $ unlines $ drop 
                (fromMaybe (crrLine edArgs) (addr1 cmd) - 1)
                (reverse (drop (length allLines - (fromMaybe 1 (addr1 cmd) + fromMaybe 1 (addr2 cmd) - 1)) $ reverse allLines)) 

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
