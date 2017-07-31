module Vihs
     ( VihsState
     , vihsRun
     , vihsTestRun
     , vihsInit
     , vihsDefault
     ) where

import Control.Monad.State
import System.Console.Haskeline
import Data.Maybe
import System.Process
import HiddenChar.HiddenChar

data VihsState = VihsState { mode   :: Mode
                           , quited :: Bool
                           , number :: Bool
                           } deriving (Show)

data FileState = FileState { path   :: FilePath
                           , buff   :: Text
                           , row    :: Row
                           , column :: Column
                           , yanked :: String
                           , saved  :: Bool
                           } deriving (Show)

type EditorState = (VihsState, FileState)
type Line   = String
type Text   = [Line]
type Row    = Int
type Column = Int
type Count  = Int
type Option = String

data Cmd = Move Direction Count
         | Insert  Char
         | Delete  Count
         | DelLine Count
         | Replace Count
         | Change  Mode
         | None    String
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

data ExCmd = Write  FilePath
           | Quit
           | To     Mode
           | Term
           | Git    Option
           | Stack  Option
           | Number Bool
             deriving (Show)

vihsInit :: VihsState
vihsInit =  VihsState { mode   = NORMAL
                      , quited = False 
                      , number = False }

fileInit           :: FilePath -> Text -> FileState
fileInit path buff =  FileState { path   = path
                                , buff   = buff
                                , row    = 0
                                , column = 0
                                , yanked = ""
                                , saved  = True }

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
currline fs =  buff fs !! row fs

filelength    :: FileState -> Int
filelength fs =  length (buff fs)

parseCmd        :: String -> EditorState -> IO Cmd
parseCmd str st =  do str' <- stream' True str
                      putStrLn ""
                      vihsPrint False st
                      case str' of
                        "j"  -> return $ Move UP    1
                        "k"  -> return $ Move DOWN  1
                        "h"  -> return $ Move LEFT  1
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

stream'              :: Bool -> String -> IO String
stream' finished str =  do ch <- getHiddenChar
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
 
loopM     :: (Monad m) => (a -> m a) -> a -> m a
loopM f a =  loopM f =<< f a

vihsRun             :: EditorState -> IO EditorState
vihsRun st@(vs, fs) =  do vihsPrint False st
                          if quited vs
                            then return st
                            else case mode vs of
                                   NORMAL    -> normalRun st
                                   EX        -> exRun     st
                                   INSERT ch -> insert ch st
                                   REPLACE   -> replace st

normalRun    :: EditorState -> IO EditorState
normalRun st =  do cmd <- parseCmd "" st
                   normal cmd `execStateT` st >>= vihsRun

normal     :: Cmd -> StateT EditorState IO ()
normal cmd =  case cmd of
                Move UP    _ -> modify $ move (+        1) id
                Move DOWN  _ -> modify $ move (subtract 1) id
                Move LEFT  _ -> modify $ move id           (subtract 1)
                Move RIGHT _ -> modify $ move id           (+        1)
                Delete     c -> modify $ delete  c
                DelLine    c -> modify $ delLine c
                Insert ch    -> get >>= (lift . insert ch) >>= put
                Replace 1    -> get >>= (lift . replace)   >>= put
                Change mode  -> modify $ to mode
                None str     -> get >>= (lift . nocmd str)

exRun    :: EditorState -> IO EditorState
exRun st =  do cmd <- fromMaybe "" 
                   <$> runInputT defaultSettings (getInputLine ":")
               putStrLn ""
               (nvs, nfs) <- ex (parseExCmd cmd) `execStateT` st >>= vihsRun
               return (nvs { mode = NORMAL }, nfs)

ex     :: ExCmd -> StateT EditorState IO ()
ex cmd =  case cmd of
            Write path -> get >>= (lift . write path . to NORMAL) >>= put
            Quit       -> modify $ quit . to NORMAL
            Term       -> get >>= (lift . term . to NORMAL)
            Git opt    -> get >>= (lift . git opt . to NORMAL)
            Stack opt  -> get >>= (lift . stack opt . to NORMAL)
            Number b   -> modify $ setnum b . to NORMAL
            To NORMAL  -> modify $ to NORMAL

move                :: (Row -> Row) -> (Column -> Column) ->
                       EditorState -> EditorState
move f1 f2 (vs, fs) =  (vs ,fs { row    = newRow $ row fs
                               , column = newColumn })
                       where newRow    :: (Row -> Row)
                             newRow    |  (f1 (row fs) <  0)
                                       || (f1 (row fs) >= filelength fs)
                                                    = id
                                       |  otherwise = f1
                             newColumn |  (f2 (column fs) < 0)
                                       || (f2 (column fs)
                                          >= length (currline fs))
                                       || (f1 (row fs) < 0)
                                       || (f1 (row fs)
                                          >= filelength fs)
                                                    = column fs
                                       |  length (buff fs !! f1 (row fs))
                                          <  length (currline fs)
                                       && length (buff fs !! f1 (row fs))
                                          <= column fs
                                                    = length (buff fs !! f1 (row fs)) - 1
                                       |  otherwise = f2 $ column fs

vihsPrint             :: Bool -> EditorState -> IO ()
vihsPrint isIns
          st@(vs, fs) =  do print st
                            (putStrLn . unlines) ((
                              if number vs
                                then zipWith (++)
                                             (map ((++"\t") . show)
                                             [1 ..])
                                else id) (fst ++ [putCursor isIns st]
                                              ++ tail snd))
                            where (fst, snd) = splitAt (row fs) (buff fs)

putCursor               :: Bool -> EditorState -> String
putCursor isIns (_, fs) =  fst ++ (if isIns
                                     then '|'
                                     else '[') 
                           : (if null snd 
                                then [']']
                                else head snd : (if isIns
                                                   then []
                                                   else [']'])
                                ++ tail snd)
                           where (fst, snd) = splitAt (column fs) (currline fs)

addLine        :: Row -> Text -> Text
addLine r buff =  take (r + 1) buff ++ [""] ++ drop (r + 1) buff

edit                 :: String -> EditorState -> EditorState
edit str
     st@(vs, fs) =  (vs, fs { buff   = fst ++ str : [] ++ tail snd
                            , column = newColumn
                            , saved  = False })
                    where (fst, snd) = splitAt (row fs) (buff fs)
                          newColumn :: Int
                          newColumn |  length str
                                       < length (currline fs) 
                                    && length str - 1
                                       < column fs
                                                 = length str - 1
                                    |  length str
                                       < length (currline fs)
                                                 = column fs
                                    |  otherwise = column fs
                                                   + length str
                                                   - length (currline fs)

delete               :: Count -> EditorState -> EditorState
delete c st@(vs, fs) =  f (vs ,fs { yanked = take c snd })
                        where (fst, snd) = splitAt (column fs) (currline fs)
                              f :: (EditorState -> EditorState)
                              f |  (null . currline) fs
                                             = id
                                |  otherwise = edit $ fst ++ drop c snd

delLine               :: Count -> EditorState -> EditorState
delLine c st@(vs, fs) =  newSt
                         where (fst, snd) = splitAt (row fs) (buff fs)
                               newSt :: EditorState
                               newSt |  length (buff fs) <= 1
                                                  = (vs, fs { buff = [""] })
                                     |  length (buff fs) - 1 < row fs
                                                  = (vs, fs { buff   = fst ++ drop c snd 
                                                            , row    = length (buff fs) - 1
                                                            , yanked = unlines $ take c snd})
                                     |  otherwise = (vs, fs { buff   = fst ++ drop c snd 
                                                            , row    = row fs
                                                            , yanked = unlines $ take c snd})

replace             :: EditorState -> IO EditorState
replace st@(vs, fs) =  do str <- replace' (column fs) (currline fs)
                          (vihsRun . edit str) (to NORMAL st)

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
                                                             (vs, fs { column = column fs - 1 })
                           _      -> do print ch
                                        insRun $ edit (fst ++ [ch] ++ snd) st
                         where (fst,  snd)  = splitAt (column fs) (currline fs)
                               (fstb, sndb) = splitAt (row fs)    (buff fs)
                               esc =(vs ,fs { buff = fstb
                                                     ++ (if last (currline fs)
                                                            == '\n'
                                                           then (++ [""])
                                                           else (++ []))
                                                     (lines (currline fs))
                                                     ++ tail sndb
                                            , row = (if last (currline fs)
                                                        == '\n'
                                                       then id
                                                       else subtract 
                                                              (if head (currline fs)
                                                                  == '\n'
                                                                 then 2
                                                                 else 1))
                                                    (row fs
                                                    + length (lines $ currline fs))
                                            , column = column fs
                                                       - (length 
                                                          . unlines
                                                          . init
                                                          . lines $ currline fs) })

insert                :: Char -> EditorState -> IO EditorState
insert ch st@(vs, fs) =  do vihsPrint True st'
                            st'' <- insRun st'
                            return $ to NORMAL st''
                            where (fstb, sndb) = splitAt (row fs) (buff fs)
                                  st' = case ch of
                                          'i' -> st
                                          'a' -> (vs
                                                 ,fs { column = column fs + 1 })
                                          'I' -> (vs
                                                 ,fs { column = 0 })
                                          'A' -> (vs
                                                 ,fs { column = length $ currline fs })
                                          'o' -> (vs
                                                 ,fs { row    = row fs
                                                     , column = length (currline fs) + 1
                                                     , buff   = fstb
                                                                ++ [currline fs ++ "\n"]
                                                                ++  tail sndb })
                                          'O' -> (vs
                                                 ,fs { row    = row fs
                                                     , column = 0
                                                     , buff   = fstb
                                                                ++ ["\n" ++ currline fs]
                                                                ++ tail sndb })

insert'            :: Column -> Line -> Line -> Line
insert' c str line =  fst ++ str ++ snd
                      where (fst, snd) = splitAt c line

quit          :: EditorState -> EditorState
quit (vs, fs) =  (vs { quited = True }, fs)

write               :: FilePath -> EditorState -> IO EditorState
write path (vs, fs) =  do writeFile path (unlines (buff fs))
                          return (vs, fs { path  = path
                                         , saved = True })

to               :: Mode -> EditorState -> EditorState
to mode (vs, fs) =  (vs { mode = mode }, fs)

setnum               :: Bool -> EditorState -> EditorState
setnum b st@(vs, fs) =  (vs { number = b }, fs)

term   :: EditorState -> IO ()
term _ =  system "$SHELL" >>= print

git       :: Option -> EditorState -> IO ()
git opt _ =  system ("git " ++ opt) >>= print

stack       :: Option -> EditorState -> IO ()
stack opt _ =  system ("stack " ++ opt) >>= print

nocmd       :: String -> EditorState -> IO ()
nocmd str _ =  putStrLn $ "No such command: \'" 
                          ++ str ++ "\'"
