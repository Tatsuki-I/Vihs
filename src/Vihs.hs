module Vihs
     ( VihsState
     , vihsRun
     , vihsTestRun
     , vihsInit
     , vihsDefault
     , stream
     , stream'
     ) where


--instance NFData TypeRep where

import Control.Monad.State
import System.Console.Haskeline
import Data.Maybe
import System.Process

--newtype TypeRep = TypeRep
--                  deriving (NFData)

data VihsState = VihsState { path   :: FilePath
                           , buff   :: Text
                           , row    :: Row
                           , column :: Column
                           , mode   :: Mode
                           , saved  :: Bool
                           , quited :: Bool
                           , number :: Bool
                           } deriving (Show)

newtype Cursor = Cursor (Int, Int)
                 deriving (Show)

type Line   = String
type Text   = [Line]
type Row    = Int
type Column = Int
type Count  = Int

data Cmd = Move Direction Count
         | Insert Char
         | Delete Count
         | DelLine Count
         | Replace Count
         | Change Mode
         | None String
           deriving (Show)

data Direction = UP
               | DOWN
               | LEFT
               | RIGHT
                 deriving (Show)

data Mode = NORMAL
          | INSERT Char
          | VISUAL
          | EX
          | REPLACE
            deriving (Show)

data ExCmd = Write FilePath
           | Quit
           | To Mode
           | Term
           | Number Bool
             deriving (Show)

vihsInit           :: FilePath -> Text -> VihsState
vihsInit path buff =  VihsState { path   = path
                                , buff   = buff
                                , row    = 0
                                , column = 0
                                , mode   = NORMAL
                                , saved  = True
                                , quited = False 
                                , number = False }

vihsDefault :: VihsState
vihsDefault =  vihsInit "vihstest.txt" 
                        [ "Hello Vihs!"
                        , "I'm 2nd line" 
                        , "I'm 3rd line"]

vihsTestRun :: IO VihsState
vihsTestRun =  vihsRun vihsDefault

currline    :: VihsState -> Line
currline st =  buff st !! row st

filelength    :: VihsState -> Int
filelength st =  length (buff st)

stream        :: String -> VihsState-> IO Cmd
stream str st =  do str' <- stream' True str
                    putStrLn ""
                    vihsPrint False st
                    case str' of
                      "j"  -> return $ Move UP 1
                      "k"  -> return $ Move DOWN 1
                      "h"  -> return $ Move LEFT 1
                      "l"  -> return $ Move RIGHT 1
                      "x"  -> return $ Delete 1
                      "r"  -> return $ Change REPLACE
                      "R"  -> return $ Replace 1
                      "i"  -> return $ Insert 'i'
                      "I"  -> return $ Insert 'I'
                      "a"  -> return $ Insert 'a'
                      "A"  -> return $ Insert 'A'
                      "o"  -> return $ Insert 'o'
                      "O"  -> return $ Insert 'O'
                      ":"  -> return $ Change EX
                      "dd" -> return $ DelLine 1
                      _   -> do print str'
                                vihsPrint False st
                                stream str' st

switcher        :: String -> Char -> String
switcher str ch =  case ch of
                     '\DEL' -> if null str
                                 then ""
                                 else init str
                     '\ESC' -> ""
                     _      -> str ++ [ch]

stream'              :: Bool -> String -> IO String
stream' finished str =  do ch <- getChar
                           return $ switcher str ch

parseCmd    :: Char -> Cmd
parseCmd ch =  case ch of
                 'j' -> Move UP 1
                 'k' -> Move DOWN 1
                 'h' -> Move LEFT 1
                 'l' -> Move RIGHT 1
                 'x' -> Delete 1
                 'r' -> Replace 1
                 'R' -> Replace 1
                 'i' -> Insert ch
                 'I' -> Insert ch
                 'a' -> Insert ch
                 'A' -> Insert ch
                 'o' -> Insert ch
                 'O' -> Insert ch
                 ':' -> Change EX
                 _   -> None [ch]

parseExCmd     :: String -> ExCmd
parseExCmd cmd =  case head (words cmd) of
                    "w"        -> Write $ words cmd !! 1
                    "write"    -> Write $ words cmd !! 1
                    "q"        -> Quit
                    "quit"     -> Quit
                    "set"      -> case words cmd !! 1 of
                                    "number"   -> Number True
                                    "nonumber" -> Number False
                    "terminal" -> Term
                    "BS"       -> To NORMAL
                    "\b"       -> To NORMAL
                    _          -> undefined
 
loopM     :: (Monad m) => (a -> m a) -> a -> m a
loopM f a =  loopM f =<< f a

vihsRun    :: VihsState -> IO VihsState
vihsRun st =  do vihsPrint False st
                 if quited st
                   then return st
                   else case mode st of
                          NORMAL    -> normalRun st
                          EX        -> exRun     st
                          INSERT ch -> insert ch st
                          REPLACE   -> replace st

normalRun    :: VihsState -> IO VihsState
normalRun st =  do cmd <- stream "" st
                   normal cmd `execStateT` st >>= vihsRun

normal     :: Cmd -> StateT VihsState IO ()
normal cmd =  case cmd of
                Move UP    _ -> modify $ move (+        1) id
                Move DOWN  _ -> modify $ move (subtract 1) id
                Move LEFT  _ -> modify $ move id           (subtract 1)
                Move RIGHT _ -> modify $ move id           (+        1)
                Delete     _ -> modify delete
                DelLine    c -> modify $ delLine c
                Insert ch    -> get >>= (lift . insert ch) >>= put
                Replace 1    -> get >>= (lift . replace)   >>= put
                Change mode  -> modify $ to mode
                None str     -> get >>= (lift . nocmd str) >>= put

exRun    :: VihsState -> IO VihsState
exRun st =  do cmd <- fromMaybe "" 
                   <$> runInputT defaultSettings (getInputLine ":")
               putStrLn ""
               newSt <- ex (parseExCmd cmd) `execStateT` st >>= vihsRun
               return $ newSt { mode = NORMAL }

ex     :: ExCmd -> StateT VihsState IO ()
ex cmd =  case cmd of
            Write path   -> get >>= (lift . write path . to NORMAL)      >>= put
            Quit         -> modify $ quit . to NORMAL
            Term         -> get >>= (lift . term . to NORMAL)      >>= put
            Number b     -> modify $ setnum b . to NORMAL
            To NORMAL    -> modify $ to NORMAL

move          :: (Row -> Row) -> (Column -> Column) -> VihsState -> VihsState
move f1 f2 st =  st { row    = if (f1 (row st) <  0)
                               || (f1 (row st) >= filelength st)
                                 then row st
                                 else f1 $ row st
                    , column = if (f2 (column st) <  0)
                               || (f2 (column st) >= length (currline st))
                               || (f1 (row st)    <  0)
                               || (f1 (row st)    >= filelength st)
                                 then column st 
                                 else if length (buff st !! f1 (row st)) 
                                         <  length (currline st)
                                      && length (buff st !! f1 (row st))
                                         <= column st
                                        then length (buff st !! f1 (row st)) - 1
                                        else f2 $ column st }

vihsPrint          :: Bool -> VihsState -> IO ()
vihsPrint isIns st =  do print st
                         putStrLn $ unlines ((if number st
                                       then zipWith (++)
                                                    (map ((++"\t") . show)
                                                    [1 ..])
                                       else id)
                                              (fst 
                                                ++ [putCursor isIns st]
                                                ++ tail snd))
                         where (fst, snd) = splitAt (row st) (buff st)

putCursor          :: Bool -> VihsState -> String
putCursor isIns st =  fst ++ (if isIns
                                then '|'
                                else '[') 
                      : (if null snd 
                           then [']']
                           else head snd : (if isIns
                                              then []
                                              else [']'])
                           ++ tail snd) --drop (column st + 1) (currline st)
                      where (fst, snd) = splitAt (column st) (currline st)

addLine        :: Row -> Text -> Text
addLine r buff =  take (r + 1) buff ++ [""] ++ drop (r + 1) buff

edit        :: String -> VihsState -> VihsState
edit str st =  st { buff   = fst ++ str : [] ++ tail snd
                  , column = if length str - 1 < column st
                               then length str - 1
                               else column st
                  , saved  = False }
               where (fst, snd) = splitAt (row st) (buff st)

delete    :: VihsState -> VihsState
delete st =  if null $ currline st
               then st
               else edit (delete' (column st) (currline st)) st

delete'        :: Column -> Line -> Line
delete' c line =  fst ++ tail snd
                  where (fst, snd) = splitAt c line

delLine      :: Count -> VihsState -> VihsState
delLine c st =  if length (buff st) <= 1
                  then st { buff = [""] }
                  else st { buff = fst ++ drop c snd 
                          , row  = if length (buff st) - 1 < row st
                                     then length (buff st) - 1
                                     else row st }
                where (fst, snd) = splitAt (row st) (buff st)

replace    :: VihsState -> IO VihsState
replace st =  do str <- replace' (column st) (currline st)
                 vihsRun $ edit str $ to NORMAL st

replace'        :: Column -> String -> IO String
replace' c buff =  do putStr "REPLACE>> "
                      ch <- getChar
                      return $ fst ++ [ch] ++ tail snd
                      where (fst, snd) = splitAt c buff

insert       :: Char -> VihsState -> IO VihsState
insert ch st =  do vihsPrint True st'
                   str' <- maybe ""
                                 (\str -> insert' (column st')
                                                  str
                                                  (currline st'))
                                 <$> runInputT defaultSettings
                                               (getInputLine "\nINSERT>> ")
                   return $ edit str' $ to NORMAL st'
                   where st' = case ch of
                                 'i' -> st
                                 'a' -> st { column = column st + 1 }
                                 'I' -> st { column = 0 }
                                 'A' -> st { column = length $ currline st }
                                 'o' -> st { row    = row st + 1
                                           , column = 0 
                                           , buff   = addLine (row st) (buff st) }
                                 'O' -> st { row    = row st
                                           , column = 0 
                                           , buff   = addLine (row st - 1) (buff st) }
        {- insert st =  do str <- fromMaybe "" 
                    <$> (runInputT defaultSettings $ getInputLine "\n> ")
                return $ edit (insert' (column st) str (currline st)) st
-}

insert'            :: Column -> String -> String -> String
insert' c str buff =  fst ++ str ++ snd-- ++ drop c buff
                      where (fst, snd) = splitAt c buff

quit    :: VihsState -> VihsState
quit st =  st { quited = True }

write         :: FilePath -> VihsState -> IO VihsState
write path st =  do writeFile path (unlines (buff st))
                    return st { path = path
                             , saved = True }

to         :: Mode -> VihsState -> VihsState
to mode st =  st { mode = mode }

setnum      :: Bool -> VihsState -> VihsState
setnum b st =  st { number = b }

term    :: VihsState -> IO VihsState
term st =  do system "$SHELL"
              return st

nocmd        :: String -> VihsState -> IO VihsState
nocmd str st =  do putStrLn $ "No such command: \'" 
                              ++ str ++ "\'"
                   return st
