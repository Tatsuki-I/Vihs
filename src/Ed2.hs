module Ed2
     ( edRun
     , edInit
     , currline
     , delete 
     , insert'
     ) where

import Control.Monad.State
import System.Console.Haskeline
import Data.Maybe
import System.Process

data EdState = EdState { path   :: FilePath
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
         | Write
         | Quit
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

edInit :: EdState
edInit =  EdState { path   = "test.txt"
                  , buff   = ["Hello ed!", "I'm 2nd line"]
                  , row    = 0
                  , column = 0
                  , mode   = NORMAL
                  , saved  = True
                  , quited = False }

currline    :: EdState -> Line
currline st =  buff st !! row st

filelength    :: EdState -> Int
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
                 'w' -> Write
                 'q' -> Quit
                 ':' -> Change EX
                 't' -> Term
                 _   -> None [ch]
 
loopM     :: (Monad m) => (a -> m a) -> a -> m a
loopM f a =  loopM f =<< f a

edRun    :: EdState -> IO EdState
edRun st =  do print st
               edPrint False st
               if quited st
                 then return st
                 else case mode st of
                        NORMAL -> do putStr "> "
                                     cmd <- getChar
                                     putStrLn ""
                                     ed (parseCmd cmd) `execStateT` st >>= edRun
                        EX     -> do putStr ":"
                                     cmd <- getChar
                                     putStrLn ""
                                     ed (parseCmd cmd) `execStateT` st >>= edRun

--edRun     :: String -> IO EdState
--edRun cmd =  mapM_ ed cmd `execStateT` edInit

ed     :: Cmd -> StateT EdState IO ()
ed cmd =  case cmd of
            Move UP    _ -> modify $ move (+        1) id
            Move DOWN  _ -> modify $ move (subtract 1) id
            Move LEFT  _ -> modify $ move id           (subtract 1)
            Move RIGHT _ -> modify $ move id           (+        1)
            Delete     _ -> modify delete
            Insert       -> get >>= (lift . insert)    >>= put
            Replace 1    -> get >>= (lift . replace)   >>= put
            Write        -> get >>= (lift . save)      >>= put
            Change EX    -> modify $ change EX
            Quit         -> modify quit
            Term         -> get >>= (lift . term)      >>= put
            None str     -> get >>= (lift . nocmd str) >>= put

move          :: (Row -> Row) -> (Column -> Column) -> EdState -> EdState
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

edPrint          :: Bool -> EdState -> IO ()
edPrint isIns st = putStrLn $ unlines $ fst ++ [putCursor isIns st]
                                        ++ tail snd
                   where (fst, snd) = splitAt (row st) (buff st)

putCursor          :: Bool -> EdState -> String
putCursor isIns st =  fst ++ (if isIns
                                then '|'
                                else '[') 
                      : head snd : (if isIns
                                      then []
                                      else [']'])
                      ++ tail snd --drop (column st + 1) (currline st)
                      where (fst, snd) = splitAt (column st) (currline st)

edit        :: String -> EdState -> EdState
edit str st =  st { buff  = fst ++ str : [] ++ tail snd --drop (row st + 1) (buff st)
                  , saved = False }
               where (fst, snd) = splitAt (row st) (buff st)

delete    :: EdState -> EdState
delete st =  edit (delete' (column st) (currline st)) st

delete'        :: Column -> String -> String
delete' c buff =  fst ++ tail snd
                  where (fst, snd) = splitAt c buff

replace :: EdState -> IO EdState
replace st =  do str <- replace' (column st) (currline st)
                 return $ edit str st

replace'        :: Column -> String -> IO String
replace' c buff =  do putStr "> "
                      ch <- getChar
                      return $ fst ++ [ch] ++ tail snd
                      where (fst, snd) = splitAt c buff

insert    :: EdState -> IO EdState
insert st =  do edPrint True st
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

quit    :: EdState -> EdState
quit st =  st { quited = True }

save    :: EdState -> IO EdState
save st =  do writeFile (path st) (unlines (buff st))
              return st { saved = True }

change :: Mode -> EdState -> EdState
change md st =  st { mode = md }

term :: EdState -> IO EdState
term st =  do system "$SHELL"
              return st

nocmd        :: String -> EdState -> IO EdState
nocmd str st =  do putStrLn $ "No such command: \'" 
                              ++ str ++ "\'"
                   return st
