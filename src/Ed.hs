module Ed where

import           ParseCmd
import           Control.Monad            (unless)
import           Data.Maybe
import           Delete
import           ReadWrite
import           Safe                     (headMay)
import           System.Console.Haskeline
import           Text.Parsec

data EdArgs = EdArgs
        { fileName :: String
        , buff     :: [String]
        , crrLine  :: Int
        , saved    :: Bool} deriving Show

fixAddr :: EdArgs -> AddrVal -> Int
fixAddr _ (AddrLine l) = l
fixAddr e (AddrCrr dl) = crrLine e + dl
fixAddr e AddrEOF      = length $ buff e

addr1 :: Command -> EdArgs -> Maybe Int
addr1 cmd edArgs = ((fixAddr edArgs . takeAddr1) <$>) (addr cmd)
    where
        takeAddr1 (AddrSingle v) = v
        takeAddr1 (AddrPair v _) = v

addr2 :: Command -> EdArgs-> Maybe Int
addr2 cmd edArgs = ((fixAddr edArgs . takeAddr2) <$>) (addr cmd)
    where
        takeAddr2 (AddrPair _ v) = v

ed :: [String] -> IO ()
ed args = do
        x <- if null args then return [] else createBuffer $ fromMaybe "" $ headMay args
        inputCmd >>= (`ed'` EdArgs (if null args then [] else fromMaybe "" $ headMay args) x 1 True)

ed' :: Command -> EdArgs -> IO ()
ed' cmd edArgs = case cmdName cmd of
        'q' ->
                  unless (saved edArgs) $ do
                          putStrLn "?"
                          newCmd <- inputCmd
                          ed' newCmd edArgs {saved = cmdName newCmd == 'q'}
        'a' ->
                  insert >>= (\x -> inputCmd >>=
                  (`ed'` edArgs {buff = iCmd (buff edArgs) x $ fromMaybe (crrLine edArgs) (addr1 cmd edArgs) + 1, saved = False}))
        'i' ->
                  insert >>= (\x -> inputCmd >>=
                  (`ed'` edArgs {buff = iCmd (buff edArgs) x $ fromMaybe (crrLine edArgs) $ addr1 cmd edArgs, saved = False}))
        'd' ->
                  inputCmd >>=
                  (`ed'` edArgs {buff = deleteLine (buff edArgs) (fromMaybe (crrLine edArgs) $ addr1 cmd edArgs) (fromMaybe 1 $ addr2 cmd edArgs), saved = False})
        'l' -> do
                  printBuff cmd edArgs $ addDll $ buff edArgs
                  inputCmd >>= (`ed'` edArgs)
        'n' -> do
                  let infNo = map show (take (length $ buff edArgs) [1, 2..])
                  printBuff cmd edArgs $ zipWith (++) (map (take 8 . (++ repeat ' ')) infNo) (addDll $ buff edArgs)
                  inputCmd >>= (`ed'` edArgs)
        'w' ->
                  if isNothing $ param cmd
                          then putStrLn "?"
                                  >> inputCmd >>= (`ed'` edArgs)
                          else buffToFile (fromJust (param cmd)) (buff edArgs)
                                  >> (print (length (unlines $ buff edArgs))
                                  >> inputCmd >>= (`ed'` edArgs {saved = True}))
        _ ->
                  putStrLn "?" >> inputCmd >>= (`ed'` edArgs)

inputCmd :: IO Command
inputCmd = setCmd . fromMaybe "" <$> runInputT defaultSettings (getInputLine "")

addDll :: [String] -> [String]
addDll = map (++"$")

printBuff :: Command -> EdArgs -> [String] -> IO ()
printBuff cmd edArgs allLines =
        putStr $ unlines $ drop
                ((fromMaybe (crrLine edArgs) $ addr1 cmd edArgs) - 1)
                (reverse (drop (length allLines - ((fromMaybe (crrLine edArgs) $ addr1 cmd edArgs) + (fromMaybe 1 $ addr2 cmd edArgs) - 1)) $ reverse allLines))

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
