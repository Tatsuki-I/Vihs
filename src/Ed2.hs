module Ed2
  ( edRun
  , edInit
  , currline
  , delete )
  where

import Control.Monad.State

data EdState = EdState { filepath :: FilePath
                       , buff     :: Text
                       , line     :: Int
                       , column   :: Int
                       , saved    :: Bool
                       } deriving (Show)

newtype Cursor = Cursor (Int, Int) deriving (Show)

type Text = [String]

edInit :: EdState
edInit =  EdState { filepath = "test.txt"
                  , buff     = ["Hello ed!", "I'm 2nd line"]
                  , line  = 0
                  , column  = 0
                  , saved    = False }

currline    :: EdState -> String
currline st =  (buff st) !! line st



edRun :: IO EdState
edRun =  do cmd <- getChar
            case cmd of
              'q' -> undefined
              _   -> edRun' cmd

edRun'     :: Char -> IO EdState
edRun' cmd =  ed cmd `execStateT` edInit
--edRun     :: String -> IO EdState
--edRun cmd =  mapM_ ed cmd `execStateT` edInit

ed     :: Char -> StateT EdState IO ()
ed cmd =  case cmd of
            'j' -> modify $ move (+        1) (\x -> x)
            'k' -> modify $ move (subtract 1) (\x -> x)
            'h' -> modify $ move (\x -> x)    (subtract 1)
            'l' -> modify $ move (\x -> x)    (+        1)
            'x' -> modify $ delete
            'q' -> undefined
            _   -> undefined

move          :: (Int -> Int) -> (Int -> Int) -> EdState -> EdState
move f1 f2 st =  st { line   = if f1 (line st)   < 0
                                 then 0
                                 else f1 $ line   st
                    , column = if f2 (column st) < 0
                                 then 0
                                 else f2 $ column st }


delete'       :: Int -> String -> String
delete' c str =  take c str ++ drop (c + 1) str

--delete    :: EdState -> EdState
--delete st =  edit (take (column st) (currline st)
--          ++ drop (column st + 1) (currline st)) st

delete    :: EdState -> EdState
delete st =  edit (delete' (column st) (currline st)) st

edit    :: String -> EdState -> EdState
edit str st =  st { buff =  take (line st)     (buff st)
                         ++ str : []
                         ++ drop (line st + 1) (buff st) }

insert            :: Int -> String -> String -> String
insert c str buff =  undefined --buff !! str
