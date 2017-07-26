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

data EdState = EdState { path   :: FilePath
                       , buff   :: Text
                       , row    :: Row
                       , column :: Column
                       , saved  :: Bool
                       , quited :: Bool
                       } deriving (Show)

newtype Cursor = Cursor (Int, Int) deriving (Show)

type Line   = String
type Text   = [Line]
type Row    = Int
type Column = Int

data Cmd = Move Direction
         | Insert
         | Delete
         | Write
         | Quit
         | None String
         deriving (Show)

data Direction = UP
               | DOWN
               | LEFT
               | RIGHT
               deriving (Show)

edInit :: EdState
edInit =  EdState { path   = "test.txt"
                  , buff   = ["Hello ed!", "I'm 2nd line"]
                  , row    = 0
                  , column = 0
                  , saved  = False
                  , quited = False }

currline    :: EdState -> Line
currline st =  buff st !! row st

filelength    :: EdState -> Int
filelength st =  length (buff st)

parseCmd :: Char -> Cmd
parseCmd ch =  case ch of
                 'j' -> Move UP
                 'k' -> Move DOWN
                 'h' -> Move LEFT
                 'l' -> Move RIGHT
                 'x' -> Delete
                 'i' -> Insert
                 'w' -> Write
                 'q' -> Quit
                 _   -> None [ch]

                 
 
loopM     :: (Monad m) => (a -> m a) -> a -> m a
loopM f a =  loopM f =<< f a

edRun    :: EdState -> IO EdState
edRun st =  do print st
               edPrint False st
               if quited st
                 then return st
                 else do cmd <- getChar
                         putStrLn ""
                         ed (parseCmd cmd) `execStateT` st >>= edRun

--edRun     :: String -> IO EdState
--edRun cmd =  mapM_ ed cmd `execStateT` edInit

ed     :: Cmd -> StateT EdState IO ()
ed cmd =  case cmd of
            Move UP    -> modify $ move (+        1) id
            Move DOWN  -> modify $ move (subtract 1) id
            Move LEFT  -> modify $ move id           (subtract 1)
            Move RIGHT -> modify $ move id           (+        1)
            Delete     -> modify delete
            Insert     -> get >>= (lift . insert) >>= put
            Write      -> get >>= (lift . save) >>= put
            Quit       -> modify quit
            None str   -> get >>= (lift . nocmd str) >>= put

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
edPrint isIns st = putStrLn $ unlines $ take (row st) (buff st) 
                                     ++ [putCursor isIns st]
                                     ++ drop ((row st) + 1) (buff st)

putCursor    :: Bool -> EdState -> String
putCursor isIns st =  take (column st) (currline st)
                   ++ (if isIns
                         then '|' : (head $ drop (column st) (currline st)) : []
                         else '[' : (head $ drop (column st) (currline st)) : ']' : [])
                   ++ drop ((column st) + 1) (currline st)

edit    :: String -> EdState -> EdState
edit str st =  st { buff =  take (row st)     (buff st)
                         ++ str : []
                         ++ drop (row st + 1) (buff st)
                  , saved = False }

delete    :: EdState -> EdState
delete st =  edit (delete' (column st) (currline st)) st

delete'        :: Column -> String -> String
delete' c buff =  take c       buff
               ++ drop (c + 1) buff

insert    :: EdState -> IO EdState
insert st =  do edPrint True st
                str' <- maybe "" (\str -> insert' (column st) str (currline st))
                              <$> runInputT defaultSettings (getInputLine "\n> ")
                return $ edit str' st
{-
insert st =  do str <- fromMaybe "" 
                    <$> (runInputT defaultSettings $ getInputLine "\n> ")
                return $ edit (insert' (column st) str (currline st)) st
-}

insert'            :: Column -> String -> String -> String
insert' c str buff =  take c buff
                   ++ str
                   ++ drop c buff

quit    :: EdState -> EdState
quit st =  st { quited = True }

save    :: EdState -> IO EdState
save st =  do writeFile (path st) (unlines (buff st))
              return st { saved = True }

nocmd        :: String -> EdState -> IO EdState
nocmd str st =  do putStrLn $  "No such command: \'" 
                                ++ str ++ "\'"
                   return st
