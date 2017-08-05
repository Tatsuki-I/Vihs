{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Vihs
     ( VihsState
     , vihsRun
     , vihsTestRun
     , vihsInit
     , vihsDefault
     ) where

import Control.Monad.State
import Control.Lens
import System.Console.Haskeline
import Data.Maybe
import System.Process
import HiddenChar.HiddenChar
import CmdParser
import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.IO as L

data VihsState = VihsState { _mode   :: Mode
                           , _row    :: Row
                           , _column :: Column
                           , _yanked :: Line
                           , _quited :: Bool
                           , _number :: Bool
                           } deriving (Show)

data FileState = FileState { _path   :: FilePath
                           , _buff   :: File
                           , _saved  :: Bool
                           } deriving (Show)

data Cmd = Move Direction Count
         | Insert  Char
         | Delete  Count
         | DelLine Count
         | Replace Count
         | Change  Mode
         | None    String
           deriving (Show)

data Direction = DOWN
               | UP
               | LEFT
               | RIGHT
                 deriving (Show)

data Mode = NORMAL
          | INSERT Char
          | VISUAL
          | EX
          | REPLACE
            deriving (Show)

data ExCmd = Write  FilePath
           | Quit
           | To     Mode
           | Term
           | Git    Option
           | Stack  Option
           | Number Bool
             deriving (Show)

type EditorState = (VihsState, [FileState])
type Line   = L.Text
type File   = [Line]
type Row    = Int
type Column = Int
type Count  = Int
type Option = String

makeLenses ''VihsState
makeLenses ''FileState

vihsInit :: VihsState
vihsInit =  VihsState { _mode   = NORMAL
                      , _row    = 0
                      , _column = 0
                      , _yanked = ""
                      , _quited = False 
                      , _number = False }

fileInit           :: FilePath -> File -> FileState
fileInit path buff =  FileState { _path   = path
                                , _buff   = buff
                                , _saved  = True }

editorInit       :: VihsState -> FileState -> EditorState
editorInit vs fs =  (vs, [fs])

vihsDefault :: EditorState
vihsDefault =  (vihsInit, [fileInit "vihstest.txt" 
                                    [ "Hello Vihs!"
                                    , "I'm 2nd line" 
                                    , "I'm 3rd line" ]])

vihsTestRun :: IO EditorState
vihsTestRun =  vihsRun vihsDefault

currline                :: EditorState -> Line
currline (vs, fs : _) =  (fs ^. buff) !! (vs ^. row)

filelength    :: FileState -> Int
filelength fs =  length $ fs ^. buff

parseCmd        :: String -> EditorState -> IO Cmd
parseCmd str st =  do str' <- stream' str
                      let (c, cmd) = parseCmd' str'
                      putStrLn ""
                      print $ parseCmd' str'
                      case cmd of
                        "j"  -> return . Move DOWN  $ fromMaybe 1 c
                        "k"  -> return . Move UP    $ fromMaybe 1 c
                        "h"  -> return . Move LEFT  $ fromMaybe 1 c
                        "l"  -> return . Move RIGHT $ fromMaybe 1 c
                        "x"  -> return . Delete $ fromMaybe 1 c
                        "r"  -> return $ Change REPLACE
                        "R"  -> return $ Replace 1
                        "i"  -> return $ Insert 'i'
                        "I"  -> return $ Insert 'I'
                        "a"  -> return $ Insert 'a'
                        "A"  -> return $ Insert 'A'
                        "o"  -> return $ Insert 'o'
                        "O"  -> return $ Insert 'O'
                        ":"  -> return $ Change EX
                        "dd" -> return . DelLine $ fromMaybe 1 c
                        _    -> do print str'
                                   vihsPrint False st
                                   parseCmd str' st

switcher        :: String -> Char -> String
switcher str ch =  case ch of
                     '\DEL' -> if null str
                                 then ""
                                 else init str
                     '\ESC' -> ""
                     _      -> str ++ [ch]

stream'     :: String -> IO String
stream' str =  do ch <- getHiddenChar
                  return $ switcher str ch

parseExCmd     :: String -> ExCmd
parseExCmd cmd =  case head (words cmd) of
                    cmd | cmd == "w"
                        , cmd == "write"
                               -> Write $ words cmd !! 1
                    cmd | cmd == "q"
                        , cmd == "quit"
                               -> Quit
                    "set"      -> Number $ case words cmd !! 1 of
                                             "number"   -> True
                                             "nonumber" -> False
                    "terminal" -> Term
                    "git"      -> Git . unwords . drop 1 $ words cmd
                    "stack"    -> Stack . unwords . drop 1 $ words cmd
                    ch | ch == "BS"
                       , ch == "\b"
                               -> To NORMAL
                    _          -> undefined
 
vihsRun            :: EditorState -> IO EditorState
vihsRun st@(vs, _) =  do vihsPrint False st
                         if vs ^. quited
                           then return st
                           else case vs ^. mode of
                                  NORMAL    -> normalRun st
                                  EX        -> exRun     st
                                  INSERT ch -> insert ch st
                                  REPLACE   -> replace st

normalRun    :: EditorState -> IO EditorState
normalRun st =  do cmd <- parseCmd "" st
                   print cmd
                   normal cmd `execStateT` st >>= vihsRun

normal     :: Cmd -> StateT EditorState IO ()
normal cmd =  case cmd of
                Move DOWN  c -> modify $ move (+        c) id
                Move UP    c -> modify $ move (subtract c) id
                Move LEFT  c -> modify $ move id           (subtract c)
                Move RIGHT c -> modify $ move id           (+        c)
                Delete     c -> modify $ delete  c
                DelLine    c -> modify $ delLine c
                Insert ch    -> get >>= (lift . insert ch) >>= put
                Replace 1    -> get >>= (lift . replace)   >>= put
                Change mode  -> modify $ toMode mode
                None str     -> get >>= (lift . nocmd str)

exRun    :: EditorState -> IO EditorState
exRun st =  do cmd <- fromMaybe "" 
                   <$> runInputT defaultSettings (getInputLine ":")
               putStrLn ""
               (nvs, nfs) <- ex (parseExCmd cmd) `execStateT` st >>= vihsRun
               return (nvs & mode .~ NORMAL, nfs)

ex     :: ExCmd -> StateT EditorState IO ()
ex cmd =  case cmd of
            Write path -> get >>= (lift . write path . toMode NORMAL) >>= put
            Quit       -> modify $ quit . toMode NORMAL
            Term       -> get >>= (lift . term . toMode NORMAL)
            Git opt    -> get >>= (lift . git opt . toMode NORMAL)
            Stack opt  -> get >>= (lift . stack opt . toMode NORMAL)
            Number b   -> modify $ setnum b . toMode NORMAL
            To NORMAL  -> modify $ toMode NORMAL

move                             :: (Row -> Row) -> (Column -> Column) ->
                                    EditorState -> EditorState
move f1 f2 st@(vs, fsl@(fs : _)) =  (vs { _row    = newRow
                                        , _column = newColumn }, fsl)
                                    where newRow    :: Row
                                          newRow    |  f1 (vs ^. row) < 0 = 0
                                                    |  f1 (vs ^. row)
                                                       >= filelength fs = filelength fs - 1
                                                    |  otherwise         = f1 $ vs ^. row
                                          newColumn |  (f2 (vs ^. column) < 0)
                                                    || (f2 (vs ^. column)
                                                       >= length (currline st))
                                                    || (f1 (vs ^. row) < 0)
                                                    || (f1 (vs ^. row)
                                                       >= filelength fs)
                                                                 = vs ^. column
                                                    |  length ((fs ^. buff) !! f1 (vs ^. row))
                                                       <  length (currline st)
                                                    && length ((fs ^. buff) !! f1 (vs ^. row))
                                                       <= vs ^. column
                                                                 = length ((fs ^. buff) !! f1 (vs ^. row)) - 1
                                                    |  otherwise = f2 $ vs ^. column

vihsPrint                       :: Bool -> EditorState -> IO ()
vihsPrint isIns
          st@(vs, fsl@(fs : _)) =  do print st
                                      (putStrLn . unlines) ((
                                        if vs ^. number
                                          then zipWith (++)
                                                       (map ((++ "\t") . show)
                                                       [1 ..])
                                          else id) (fst ++ [putCursor isIns st]
                                                        ++ tail snd))
                                      where (fst, snd) = splitAt (vs ^. row) (fs ^. buff)

putCursor                  :: Bool -> EditorState -> String
putCursor isIns st@(vs, _) =  fst ++ (if isIns
                                        then '|'
                                        else '[') 
                              : (if null snd 
                                   then [']']
                                   else head snd : (if isIns
                                                      then []
                                                      else [']'])
                                   ++ tail snd)
                              where (fst, snd) = splitAt (vs ^. column) (currline st)

addLine        :: Row -> File -> File
addLine r buff =  take (r + 1) buff ++ [""] ++ drop (r + 1) buff

edit                           :: String -> EditorState -> EditorState
edit str st@(vs, fsl@(fs : _)) =  (vs & column .~ newColumn
                                  ,fs { _buff  = fst ++ str : [] ++ tail snd
                                      , _saved = False } : fsl)
                                  where (fst, snd) = splitAt (vs ^. row) (fs ^. buff)
                                        newColumn :: Int
                                        newColumn |  length str
                                                     < length (currline st) 
                                                  && length str - 1
                                                     < vs ^. column
                                                               = length str - 1
                                                  |  length str
                                                     < length (currline st)
                                                               = vs ^. column
                                                  |  otherwise = vs ^. column
                                                                 + length str
                                                                 - length (currline st)

delete                         :: Count -> EditorState -> EditorState
delete c st@(vs, fsl@(fs : _)) =  f (vs & yanked .~ take c snd, fsl)
                                  where (fst, snd) = splitAt (vs ^. column) (currline st)
                                        f :: (EditorState -> EditorState)
                                        f |  (null . currline) st
                                                       = id
                                          |  otherwise = edit $ fst ++ drop c snd

delLine                      :: Count -> EditorState -> EditorState
delLine c (vs, fsl@(fs : _)) =  newSt
                                where (fst, snd) = splitAt (vs ^. row) (fs ^. buff)
                                      newSt :: EditorState
                                      newSt |  length (fs ^. buff) <= 1
                                                         = (vs, (fs & buff .~ [""]) : fsl)
                                            |  length (fs ^. buff) - 1 < vs ^. row
                                                         = (vs { _row    = length (fs ^. buff) - 1
                                                               , _yanked = unlines $ take c snd }
                                                           ,(fs & buff .~ fst ++ drop c snd) : fsl)
                                            |  otherwise = (vs { _row    = vs ^. row
                                                               , _yanked = unlines $ take c snd }
                                                           ,(fs & buff .~ fst ++ drop c snd) : fsl)

replace                       :: EditorState -> IO EditorState
replace st@(vs, fsl@(fs : _)) =  do str <- replace' (vs ^. column) (currline st)
                                    (vihsRun . edit str) (toMode NORMAL st)

replace'        :: Column -> String -> IO String
replace' c buff =  do putStr "REPLACE>> "
                      ch <- getHiddenChar
                      return $ fst ++ [ch] ++ tail snd
                      where (fst, snd) = splitAt c buff

insRun                       :: EditorState -> IO EditorState
insRun st@(vs, fsl@(fs : _)) =  do vihsPrint True st
                                   ch <- getHiddenChar
                                   putStrLn ""
                                   case ch of
                                     '\ESC' -> return esc
                                     ch | ch == '\DEL'
                                        , ch == '\b' 
                                            -> do print ch
                                                  insRun $ if null fst
                                                             then st
                                                             else edit (init fst ++ snd)
                                                                       (vs & column .~ (vs ^. column - 1), fsl)
                                     _      -> do print ch
                                                  insRun $ edit (fst ++ [ch] ++ snd) st
                                   where (fst,  snd)  = splitAt (vs ^. column) (currline st)
                                         (fstb, sndb) = splitAt (vs ^. row)    (fs ^. buff)
                                         esc :: EditorState
                                         esc =  (vs { _row = (if last (currline st)
                                                                 == '\n'
                                                                then id
                                                                else subtract 
                                                                       (if head (currline st)
                                                                           == '\n'
                                                                          then 2
                                                                          else 1))
                                                             (vs ^. row
                                                             + length (lines $ currline st))
                                                    , _column = vs ^. column
                                                               - (length 
                                                                  . unlines
                                                                  . init
                                                                  . lines $ currline st) }
                                                ,(fs & buff .~ (fstb
                                                              ++ (if last (currline st)
                                                                     == '\n'
                                                                    then (++ [""])
                                                                    else (++ []))
                                                              (lines (currline st))
                                                              ++ tail sndb)) : fsl)

insert                          :: Char -> EditorState -> IO EditorState
insert ch st@(vs, fsl@(fs : _)) =  do vihsPrint True st'
                                      st'' <- insRun st'
                                      return $ toMode NORMAL st''
                                      where (fstb, sndb) = splitAt (vs ^. row) (fs ^. buff)
                                            st' :: EditorState
                                            st' =  case ch of
                                                     'i' -> (vs, fsl)
                                                     'a' -> (vs & column .~ (vs ^. column + 1), fsl)
                                                     'I' -> (vs & column .~ 0, fsl)
                                                     'A' -> (vs & column .~ length (currline st), fsl)
                                                     'o' -> (vs { _row    = vs ^. row
                                                                , _column = length (currline st) + 1 }
                                                            ,(fs & buff .~ (fstb
                                                                           ++ [currline st ++ "\n"]
                                                                           ++  tail sndb)) : fsl)
                                                     'O' -> (vs { _row    = vs ^. row
                                                                , _column = 0 }
                                                            ,(fs & buff .~ (fstb
                                                                            ++ ["\n" ++ currline st]
                                                                            ++ tail sndb)) : fsl)

insert'            :: Column -> Line -> Line -> Line
insert' c str line =  splitAt c line ^. _1 ++ str ++ splitAt c line ^. _2

quit           :: EditorState -> EditorState
quit (vs, fsl) =  (vs & quited .~ True, fsl)

write                         :: FilePath -> EditorState -> IO EditorState
write path (vs, fsl@(fs : _)) =  do writeFile path . unlines $ fs ^. buff
                                    return (vs, fs { _path  = path
                                                   , _saved = True } : fsl)

toMode            :: Mode -> EditorState -> EditorState
toMode m (vs, fs) =  (vs & mode .~ m, fs)

setnum            :: Bool -> EditorState -> EditorState
setnum b (vs, fs) =  (vs & number .~ b, fs)

term   :: EditorState -> IO ()
term _ =  system "$SHELL" >>= print

git       :: Option -> EditorState -> IO ()
git opt _ =  system ("git " ++ opt) >>= print

stack       :: Option -> EditorState -> IO ()
stack opt _ =  system ("stack " ++ opt) >>= print

nocmd       :: String -> EditorState -> IO ()
nocmd str _ =  putStrLn $ "No such command: \'" 
                          ++ str ++ "\'"
