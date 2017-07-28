module Vihs
     ( vihsRun
     , vihsInit
     , currline
     , delete 
     , insert'
     ) where

import Control.Monad.State
import System.Console.Haskeline
import Data.Maybe
import System.Process

data VihsState = VihsState { path   :: FilePath
                           , buff   :: Text
                           , row    :: Row
                           , column :: Column
                           , mode   :: Mode
                           , saved  :: Bool
                           , quited :: Bool
                           } deriving (Show)

newtype Cursor = Cursor (Int, Int)
                 deriving (Show)

type Line   = String
type Text   = [Line]
type Row    = Int
type Column = Int
type Count  = Word

data Cmd = Move Direction Count
         | Insert
         | Delete Count
         | Replace Count
         | Change Mode
         | Term
         | None String
           deriving (Show)

data Direction = UP
               | DOWN
               | LEFT
               | RIGHT
                 deriving (Show)

data Mode = NORMAL
          | INSERT InsCmd
          | VISUAL
          | EX
            deriving (Show)

data InsCmd = A CASE
            | I
            | O
              deriving (Show)

data CASE = Upper
          | Lower
            deriving (Show)

data ExCmd = Write
           | Quit
             deriving (Show)

vihsInit :: VihsState
vihsInit =  VihsState { path   = "test.txt"
                      , buff   = ["Hello Vihs!", "I'm 2nd line"]
                      , row    = 0
                      , column = 0
                      , mode   = NORMAL
                      , saved  = True
                      , quited = False }

currline    :: VihsState -> Line
currline st =  buff st !! row st

filelength    :: VihsState -> Int
filelength st =  length (buff st)

parseCmd    :: Char -> Cmd
parseCmd ch =  case ch of
                 'j' -> Move UP 1
                 'k' -> Move DOWN 1
                 'h' -> Move LEFT 1
                 'l' -> Move RIGHT 1
                 'x' -> Delete 1
                 'r' -> Replace 1
                 'R' -> Replace 1
                 'i' -> Insert
                 ':' -> Change EX
                 't' -> Term
                 _   -> None [ch]

parseExCmd     :: String -> ExCmd
parseExCmd cmd =  case head (words cmd) of
                    "w" -> Write
                    "write" -> Write
                    "quit" -> Quit
 
loopM     :: (Monad m) => (a -> m a) -> a -> m a
loopM f a =  loopM f =<< f a

vihsRun    :: VihsState -> IO VihsState
vihsRun st =  do print st
                 vihsPrint False st
                 if quited st
                   then return st
                   else case mode st of
                          NORMAL -> do putStr "> "
                                       cmd <- getChar
                                       putStrLn ""
                                       normal (parseCmd cmd) `execStateT` st >>= vihsRun
                          EX     -> do putStr ":"
                                       cmd <- getLine
                                       putStrLn ""
                                       ex (parseExCmd cmd) `execStateT` st >>= vihsRun

--edRun     :: String -> IO VihsState
--edRun cmd =  mapM_ ed cmd `execStateT` edInit

normal     :: Cmd -> StateT VihsState IO ()
normal cmd =  case cmd of
                Move UP    _ -> modify $ move (+        1) id
                Move DOWN  _ -> modify $ move (subtract 1) id
                Move LEFT  _ -> modify $ move id           (subtract 1)
                Move RIGHT _ -> modify $ move id           (+        1)
                Delete     _ -> modify delete
                Insert       -> get >>= (lift . insert)    >>= put
                Replace 1    -> get >>= (lift . replace)   >>= put
                Change EX    -> modify $ change EX
                Term         -> get >>= (lift . term)      >>= put
                None str     -> get >>= (lift . nocmd str) >>= put

ex     :: ExCmd -> StateT VihsState IO ()
ex cmd =  case cmd of
            Write        -> get >>= (lift . save)      >>= put
            Quit         -> modify quit

move          :: (Row -> Row) -> (Column -> Column) -> VihsState -> VihsState
move f1 f2 st =  st { row    = if (f1 (row st) < 0)
                               || (f1 (row st) == filelength st)
                                 then row st
                                 else f1 $ row st
                    , column = if (f2 (column st) < 0)
                               || (f2 (column st) == length (currline st))
                               || (f1 (row st)    < 0)
                               || (f1 (row st)    == filelength st)
                                 then column st 
                                 else if length (buff st !! f1 (row st)) 
                                       < length (currline st)
                                        then length (buff st !! f1 (row st)) - 1
                                        else f2 $ column st }

vihsPrint          :: Bool -> VihsState -> IO ()
vihsPrint isIns st = putStrLn $ unlines $ fst ++ [putCursor isIns st]
                                          ++ tail snd
                     where (fst, snd) = splitAt (row st) (buff st)

putCursor          :: Bool -> VihsState -> String
putCursor isIns st =  fst ++ (if isIns
                                then '|'
                                else '[') 
                      : head snd : (if isIns
                                      then []
                                      else [']'])
                      ++ tail snd --drop (column st + 1) (currline st)
                      where (fst, snd) = splitAt (column st) (currline st)

edit        :: String -> VihsState -> VihsState
edit str st =  st { buff  = fst ++ str : [] ++ tail snd --drop (row st + 1) (buff st)
                  , saved = False }
               where (fst, snd) = splitAt (row st) (buff st)

delete    :: VihsState -> VihsState
delete st =  edit (delete' (column st) (currline st)) st

delete'        :: Column -> String -> String
delete' c buff =  fst ++ tail snd
                  where (fst, snd) = splitAt c buff

replace :: VihsState -> IO VihsState
replace st =  do str <- replace' (column st) (currline st)
                 return $ edit str st

replace'        :: Column -> String -> IO String
replace' c buff =  do putStr "REPLACE>> "
                      ch <- getChar
                      return $ fst ++ [ch] ++ tail snd
                      where (fst, snd) = splitAt c buff

insert    :: VihsState -> IO VihsState
insert st =  do vihsPrint True st
                str' <- maybe "" (\str -> insert' (column st) str (currline st))
                              <$> runInputT defaultSettings (getInputLine "\nINSERT>> ")
                return $ edit str' st
{-
insert st =  do str <- fromMaybe "" 
                    <$> (runInputT defaultSettings $ getInputLine "\n> ")
                return $ edit (insert' (column st) str (currline st)) st
-}

insert'            :: Column -> String -> String -> String
insert' c str buff =  fst ++ str ++ snd-- ++ drop c buff
                      where (fst, snd) = splitAt c buff

quit    :: VihsState -> VihsState
quit st =  st { quited = True }

save    :: VihsState -> IO VihsState
save st =  do writeFile (path st) (unlines (buff st))
              return st { saved = True }

change :: Mode -> VihsState -> VihsState
change md st =  st { mode = md }

term :: VihsState -> IO VihsState
term st =  do system "$SHELL"
              return st

nocmd        :: String -> VihsState -> IO VihsState
nocmd str st =  do putStrLn $ "No such command: \'" 
                              ++ str ++ "\'"
                   return st
