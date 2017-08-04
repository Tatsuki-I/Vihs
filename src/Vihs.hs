{-# LANGUAGE TemplateHaskell #-}

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

data VihsState = VihsState { _mode   :: Mode
                           , _quited :: Bool
                           , _number :: Bool
                           } deriving (Show)


data FileState = FileState { _path   :: FilePath
                           , _buff   :: Text
                           , _row    :: Row
                           , _column :: Column
                           , _yanked :: String
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

type EditorState = (VihsState, FileState)
type Line   = String
type Text   = [Line]
type Row    = Int
type Column = Int
type Count  = Int
type Option = String

makeLenses ''VihsState
makeLenses ''FileState

vihsInit :: VihsState
vihsInit =  VihsState { _mode   = NORMAL
                      , _quited = False 
                      , _number = False }

fileInit           :: FilePath -> Text -> FileState
fileInit path buff =  FileState { _path   = path
                                , _buff   = buff
                                , _row    = 0
                                , _column = 0
                                , _yanked = ""
                                , _saved  = True }

editorInit       :: VihsState -> FileState -> EditorState
editorInit vs fs =  (vs, fs)

vihsDefault :: EditorState
vihsDefault =  (vihsInit, fileInit "vihstest.txt" 
                                   [ "Hello Vihs!"
                                   , "I'm 2nd line" 
                                   , "I'm 3rd line"])

vihsTestRun :: IO EditorState
vihsTestRun =  vihsRun vihsDefault

currline    :: FileState -> Line
currline fs =  (fs ^. buff) !! (fs ^. row)

filelength    :: FileState -> Int
filelength fs =  length $ fs ^. buff

parseCmd        :: String -> EditorState -> IO Cmd
parseCmd str st =  do str' <- stream' str
                      let (c, cmd) = parseCmd' str'
                      putStrLn ""
                      print $ parseCmd' str'
                      case cmd of
                        "j"  -> return $ Move DOWN  (fromMaybe 1 c)
                        "k"  -> return $ Move UP    (fromMaybe 1 c)
                        "h"  -> return $ Move LEFT  (fromMaybe 1 c)
                        "l"  -> return $ Move RIGHT (fromMaybe 1 c)
                        "x"  -> return $ Delete (fromMaybe 1 c)
                        "r"  -> return $ Change REPLACE
                        "R"  -> return $ Replace 1
                        "i"  -> return $ Insert 'i'
                        "I"  -> return $ Insert 'I'
                        "a"  -> return $ Insert 'a'
                        "A"  -> return $ Insert 'A'
                        "o"  -> return $ Insert 'o'
                        "O"  -> return $ Insert 'O'
                        ":"  -> return $ Change EX
                        "dd" -> return $ DelLine (fromMaybe 1 c)
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

move                :: (Row -> Row) -> (Column -> Column) ->
                       EditorState -> EditorState
move f1 f2 (vs, fs) =  (vs, fs { _row    = newRow
                               , _column = newColumn })
                       where newRow    :: Row
                             newRow    |  f1 (fs ^. row) < 0 = 0
                                       |  f1 (fs ^. row)
                                          >= filelength fs = filelength fs - 1
                                       |  otherwise         = f1 $ fs ^. row
                             newColumn |  (f2 (fs ^. column) < 0)
                                       || (f2 (fs ^. column)
                                          >= length (currline fs))
                                       || (f1 (fs ^. row) < 0)
                                       || (f1 (fs ^. row)
                                          >= filelength fs)
                                                    = fs ^. column
                                       |  length ((fs ^. buff) !! f1 (fs ^. row))
                                          <  length (currline fs)
                                       && length ((fs ^. buff) !! f1 (fs ^. row))
                                          <= fs ^. column
                                                    = length ((fs ^. buff) !! f1 (fs ^. row)) - 1
                                       |  otherwise = f2 $ fs ^. column

vihsPrint             :: Bool -> EditorState -> IO ()
vihsPrint isIns
          st@(vs, fs) =  do print st
                            (putStrLn . unlines) ((
                              if vs ^. number
                                then zipWith (++)
                                             (map ((++"\t") . show)
                                             [1 ..])
                                else id) (fst ++ [putCursor isIns fs]
                                              ++ tail snd))
                            where (fst, snd) = splitAt (fs ^. row) (fs ^. buff)

putCursor          :: Bool -> FileState -> String
putCursor isIns fs =  fst ++ (if isIns
                                then '|'
                                else '[') 
                      : (if null snd 
                           then [']']
                           else head snd : (if isIns
                                              then []
                                              else [']'])
                           ++ tail snd)
                      where (fst, snd) = splitAt (fs ^. column) (currline fs)

addLine        :: Row -> Text -> Text
addLine r buff =  take (r + 1) buff ++ [""] ++ drop (r + 1) buff

edit              :: String -> EditorState -> EditorState
edit str (vs, fs) =  (vs, fs { _buff   = fst ++ str : [] ++ tail snd
                             , _column = newColumn
                             , _saved  = False })
                     where (fst, snd) = splitAt (fs ^. row) (fs ^. buff)
                           newColumn :: Int
                           newColumn |  length str
                                        < length (currline fs) 
                                     && length str - 1
                                        < fs ^. column
                                                  = length str - 1
                                     |  length str
                                        < length (currline fs)
                                                  = fs ^. column
                                     |  otherwise = fs ^. column
                                                    + length str
                                                    - length (currline fs)

delete               :: Count -> EditorState -> EditorState
delete c st@(vs, fs) =  f (vs ,fs & yanked .~ take c snd)
                        where (fst, snd) = splitAt (fs ^. column) (currline fs)
                              f :: (EditorState -> EditorState)
                              f |  (null . currline) fs
                                             = id
                                |  otherwise = edit $ fst ++ drop c snd

delLine            :: Count -> EditorState -> EditorState
delLine c (vs, fs) =  newSt
                      where (fst, snd) = splitAt (fs ^. row) (fs ^. buff)
                            newSt :: EditorState
                            newSt |  length (fs ^. buff) <= 1
                                               = (vs, fs & buff .~ [""])
                                  |  length (fs ^. buff) - 1 < fs ^. row
                                               = (vs, fs { _buff   = fst ++ drop c snd 
                                                         , _row    = length (fs ^. buff) - 1
                                                         , _yanked = unlines $ take c snd})
                                  |  otherwise = (vs, fs { _buff   = fst ++ drop c snd 
                                                         , _row    = fs ^. row
                                                         , _yanked = unlines $ take c snd})

replace            :: EditorState -> IO EditorState
replace st@(_, fs) =  do str <- replace' (fs ^. column) (currline fs)
                         (vihsRun . edit str) (toMode NORMAL st)

replace'        :: Column -> String -> IO String
replace' c buff =  do putStr "REPLACE>> "
                      ch <- getHiddenChar
                      return $ fst ++ [ch] ++ tail snd
                      where (fst, snd) = splitAt c buff

insRun             :: EditorState -> IO EditorState
insRun st@(vs, fs) =  do vihsPrint True st
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
                                                             (vs, fs { _column = fs^.column - 1 })
                           _      -> do print ch
                                        insRun $ edit (fst ++ [ch] ++ snd) st
                         where (fst,  snd)  = splitAt (fs ^. column) (currline fs)
                               (fstb, sndb) = splitAt (fs ^. row)    (fs^.buff)
                               esc =(vs, fs { _buff = fstb
                                                     ++ (if last (currline fs)
                                                            == '\n'
                                                           then (++ [""])
                                                           else (++ []))
                                                     (lines (currline fs))
                                                     ++ tail sndb
                                            , _row = (if last (currline fs)
                                                        == '\n'
                                                       then id
                                                       else subtract 
                                                              (if head (currline fs)
                                                                  == '\n'
                                                                 then 2
                                                                 else 1))
                                                    (fs ^. row
                                                    + length (lines $ currline fs))
                                            , _column = fs ^. column
                                                       - (length 
                                                          . unlines
                                                          . init
                                                          . lines $ currline fs) })

insert             :: Char -> EditorState -> IO EditorState
insert ch (vs, fs) =  do vihsPrint True st'
                         st'' <- insRun st'
                         return $ toMode NORMAL st''
                         where (fstb, sndb) = splitAt (fs ^. row) (fs ^. buff)
                               st' = case ch of
                                       'i' -> (vs, fs)
                                       'a' -> (vs, fs & column .~ (fs ^. column + 1))
                                       'I' -> (vs, fs & column .~ 0)
                                       'A' -> (vs, fs & column .~ length (currline fs))
                                       'o' -> (vs, fs { _row    = fs ^. row
                                                      , _column = length (currline fs) + 1
                                                      , _buff   = fstb
                                                                 ++ [currline fs ++ "\n"]
                                                                 ++  tail sndb })
                                       'O' -> (vs, fs { _row    = fs ^. row
                                                      , _column = 0
                                                      , _buff   = fstb
                                                                 ++ ["\n" ++ currline fs]
                                                                 ++ tail sndb })

insert'            :: Column -> Line -> Line -> Line
insert' c str line =  (splitAt c line) ^. _1 ++ str ++ (splitAt c line) ^. _2

quit          :: EditorState -> EditorState
quit (vs, fs) =  (vs&quited.~True, fs)

write               :: FilePath -> EditorState -> IO EditorState
write path (vs, fs) =  do writeFile path . unlines $ fs ^. buff
                          return (vs, fs { _path  = path
                                         , _saved = True })

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
