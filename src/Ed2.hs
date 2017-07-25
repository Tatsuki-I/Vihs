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
                       , line   :: Int
                       , column :: Int
                       , saved  :: Bool
                       } deriving (Show)

newtype Cursor = Cursor (Int, Int) deriving (Show)

type Text = [String]

edInit :: EdState
edInit =  EdState { path   = "test.txt"
                  , buff   = ["Hello ed!", "I'm 2nd line"]
                  , line   = 0
                  , column = 0
                  , saved  = False }

currline    :: EdState -> String
currline st =  buff st !! line st

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
            'j' -> modify $ move (+        1) id
            'k' -> modify $ move (subtract 1) id
            'h' -> modify $ move id           (subtract 1)
            'l' -> modify $ move id           (+        1)
            'x' -> modify delete
            'i' -> get >>= (lift . insert) >>= put
            'q' -> undefined
            _   -> undefined

move          :: (Int -> Int) -> (Int -> Int) -> EdState -> EdState
move f1 f2 st =  st { line   = if f1 (line st)   < 0
                                 then 0
                                 else f1 $ line   st
                    , column = if f2 (column st) < 0
                                 then 0
                                 else f2 $ column st }

edit    :: String -> EdState -> EdState
edit str st =  st { buff =  take (line st)     (buff st)
                         ++ str : []
                         ++ drop (line st + 1) (buff st) }

delete    :: EdState -> EdState
delete st =  edit (delete' (column st) (currline st)) st

delete'        :: Int -> String -> String
delete' c buff =  take c       buff
               ++ drop (c + 1) buff

insert        :: EdState -> IO EdState
--insert st =  do str <- fromMaybe "" 
--                    <$> (runInputT defaultSettings $ getInputLine "\n> ")
--                return $ edit (insert' (column st) str (currline st)) st
insert st = return (edit (maybe "" 
                                (\str -> (insert' (column st) str (currline st)))
                                <$> (runInputT defaultSettings $ getInputLine "\n> ")) 
                         st)

insert'            :: Int -> String -> String -> String
insert' c str buff =  take c buff 
                   ++ str 
                   ++ drop c buff
