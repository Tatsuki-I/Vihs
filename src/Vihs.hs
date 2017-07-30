module Vihs
     ( VihsState
     , vihsRun
     , vihsTestRun
     , vihsInit
     , vihsDefault
     ) where


--instance NFData TypeRep where

import Control.Monad.State
import System.Console.Haskeline
import Data.Maybe
import System.Process
import HiddenChar.HiddenChar

--newtype TypeRep = TypeRep
--                  deriving (NFData)

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

newtype Cursor = Cursor (Int, Int)
                 deriving (Show)

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

data ExCmd = Write FilePath
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

editorInit       :: VihsState -> FileState -> (VihsState, FileState)
editorInit vs fs =  (vs, fs)

vihsDefault :: (VihsState, FileState)
vihsDefault =  (vihsInit ,fileInit "vihstest.txt" 
                                   [ "Hello Vihs!"
                                   , "I'm 2nd line" 
                                   , "I'm 3rd line"])

vihsTestRun :: IO (VihsState, FileState)
vihsTestRun =  vihsRun vihsDefault

currline    :: FileState -> Line
currline fs =  buff fs !! row fs

filelength    :: FileState -> Int
filelength fs =  length (buff fs)

parseCmd        :: String -> (VihsState, FileState) -> IO Cmd
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
                        _   -> do print str'
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
                    "w"        -> Write $ words cmd !! 1
                    "write"    -> Write $ words cmd !! 1
                    "q"        -> Quit
                    "quit"     -> Quit
                    "set"      -> Number $ case words cmd !! 1 of
                                             "number"   -> True
                                             "nonumber" -> False
                    "terminal" -> Term
                    "git"      -> Git . unwords . drop 1 $ words cmd
                    "stack"    -> Stack . unwords . drop 1 $ words cmd
                    "BS"       -> To NORMAL
                    "\b"       -> To NORMAL
                    _          -> undefined
 
loopM     :: (Monad m) => (a -> m a) -> a -> m a
loopM f a =  loopM f =<< f a

vihsRun             :: (VihsState, FileState) -> IO (VihsState, FileState)
vihsRun st@(vs, fs) =  do vihsPrint False st
                          if quited vs
                            then return st
                            else case mode vs of
                                   NORMAL    -> normalRun st
                                   EX        -> exRun     st
                                   INSERT ch -> insert ch st
                                   REPLACE   -> replace st

normalRun    :: (VihsState, FileState) -> IO (VihsState, FileState)
normalRun st =  do cmd <- parseCmd "" st
                   normal cmd `execStateT` st >>= vihsRun

normal     :: Cmd -> StateT (VihsState, FileState) IO ()
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

exRun    :: (VihsState, FileState) -> IO (VihsState, FileState)
exRun st =  do cmd <- fromMaybe "" 
                   <$> runInputT defaultSettings (getInputLine ":")
               putStrLn ""
               (nvs, nfs) <- ex (parseExCmd cmd) `execStateT` st >>= vihsRun
               return $ (nvs { mode = NORMAL }, nfs)

ex     :: ExCmd -> StateT (VihsState, FileState) IO ()
ex cmd =  case cmd of
            Write path   -> get >>= (lift . write path . to NORMAL) >>= put
            Quit         -> modify $ quit . to NORMAL
            Term         -> get >>= (lift . term . to NORMAL)
            Git opt      -> get >>= (lift . git opt . to NORMAL)
            Stack opt    -> get >>= (lift . stack opt . to NORMAL)
            Number b     -> modify $ setnum b . to NORMAL
            To NORMAL    -> modify $ to NORMAL

move                :: (Row -> Row) -> (Column -> Column) ->
                       (VihsState, FileState) -> (VihsState, FileState)
move f1 f2 (vs, fs) =  (vs,
                       fs { row    = (if (f1 (row fs) <  0)
                                      || (f1 (row fs) >= filelength fs)
                                        then id
                                        else f1) $ row fs
                          , column = if (f2 (column fs) <  0)
                                     || (f2 (column fs) >= length (currline fs))
                                     || (f1 (row fs)    <  0)
                                     || (f1 (row fs)    >= filelength fs)
                                       then column fs 
                                       else if length (buff fs !! f1 (row fs)) 
                                               <  length (currline fs)
                                            && length (buff fs !! f1 (row fs))
                                               <= column fs
                                              then length (buff fs !! f1 (row fs)) - 1
                                              else f2 $ column fs })

vihsPrint             :: Bool -> (VihsState, FileState) -> IO ()
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

putCursor               :: Bool -> (VihsState, FileState) -> String
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

edit                 :: String -> (VihsState, FileState)
                        -> (VihsState, FileState)
edit str st@(vs, fs) =  (vs, fs { buff   = fst ++ str : [] ++ tail snd
                                , column = if length str < length (currline fs)
                                             then if length str - 1 < column fs
                                               then length str - 1
                                               else column fs
                                             else column fs
                                                  + length str
                                                  - length (currline fs)
                                , saved  = False })
                        where (fst, snd) = splitAt (row fs) (buff fs)

delete               :: Count -> (VihsState, FileState)
                        -> (VihsState, FileState)
delete c st@(vs, fs) =  (if (null . currline) fs
                           then id
                           else edit $ fst ++ drop c snd) (vs
                                                          ,fs { yanked = take c snd })
                        where (fst, snd) = splitAt (column fs) (currline fs)

delLine               :: Count -> (VihsState, FileState)
                         -> (VihsState, FileState)
delLine c st@(vs, fs) =  if length (buff fs) <= 1
                           then (vs, fs { buff = [""] })
                           else (vs, fs { buff = fst ++ drop c snd 
                                        , row  = if length (buff fs) - 1 
                                                    < row fs
                                                   then length (buff fs) - 1
                                                   else row fs
                                        , yanked = unlines $ take c snd})
                         where (fst, snd) = splitAt (row fs) (buff fs)

replace             :: (VihsState, FileState) -> IO (VihsState, FileState)
replace st@(vs, fs) =  do str <- replace' (column fs) (currline fs)
                          (vihsRun . edit str) (to NORMAL st)

replace'        :: Column -> String -> IO String
replace' c buff =  do putStr "REPLACE>> "
                      ch <- getHiddenChar
                      return $ fst ++ [ch] ++ tail snd
                      where (fst, snd) = splitAt c buff

insRun    :: (VihsState, FileState) -> IO (VihsState, FileState)
insRun st@(vs, fs) =  do vihsPrint True st
                         ch <- getHiddenChar
                         putStrLn ""
                         case ch of
                           '\ESC' -> return (vs
                                            ,fs { buff = fstb
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
                           ch | ch =='\DEL'
                              , ch == '\b' -> do print ch
                                                 insRun $ if null fst
                                                            then st
                                                            else edit (init fst ++ snd)
                                                                      (vs, fs { column = column fs - 1 })
                           _      -> do print ch
                                        insRun $ edit (fst ++ [ch] ++ snd) st
                         where (fst,  snd)  = splitAt (column fs) (currline fs)
                               (fstb, sndb) = splitAt (row fs)    (buff fs)

insert       :: Char -> (VihsState, FileState)
                -> IO (VihsState, FileState)
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
insert' c str line =  fst ++ str ++ snd-- ++ drop c line
                      where (fst, snd) = splitAt c line

quit          :: (VihsState, FileState) -> (VihsState, FileState)
quit (vs, fs) =  (vs { quited = True }, fs)

write         :: FilePath -> (VihsState, FileState) -> IO (VihsState, FileState)
write path (vs, fs) =  do writeFile path (unlines (buff fs))
                          return (vs
                                 ,fs { path  = path
                                      , saved = True })

to               :: Mode -> (VihsState, FileState)
                    -> (VihsState, FileState)
to mode (vs, fs) =  (vs { mode = mode }, fs)

setnum               :: Bool -> (VihsState, FileState) -> (VihsState, FileState)
setnum b st@(vs, fs) =  (vs { number = b }, fs)

term   :: (VihsState, FileState) -> IO ()
term _ =  system "$SHELL" >>= print

git       :: Option -> (VihsState, FileState) -> IO ()
git opt _ =  system ("git " ++ opt) >>= print

stack       :: Option -> (VihsState, FileState) -> IO ()
stack opt _ =  system ("stack " ++ opt) >>= print

nocmd       :: String -> (VihsState, FileState) -> IO ()
nocmd str _ =  putStrLn $ "No such command: \'" 
                          ++ str ++ "\'"
