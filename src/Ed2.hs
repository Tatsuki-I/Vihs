module Ed2
  ( edRun )
  where

import Control.Monad.State

data EdState = EdState { filepath :: FilePath
                       , buff     :: Text
                       , cursorX  :: Int
                       , cursorY  :: Int
                       , saved    :: Bool
                       } deriving (Show)

newtype Cursor = Cursor (Int, Int) deriving (Show)

type Text = [String]

edInit :: EdState
edInit =  EdState { filepath = "test.txt"
                  , buff     = ["Hello ed!"]
                  , cursorX  = 0
                  , cursorY  = 0
                  , saved    = False }

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
            'j' -> modify $ moveC (+        1) (\x -> x)
            'k' -> modify $ moveC (subtract 1) (\x -> x)
            'h' -> modify $ moveC (\x -> x)    (subtract 1)
            'l' -> modify $ moveC (\x -> x)    (+        1)
            'q' -> undefined
            _   -> undefined

moveC          :: (Int -> Int) -> (Int -> Int) -> EdState -> EdState
moveC f1 f2 st =  st { cursorX = if f1 (cursorX st) < 0
                                   then 0
                                   else f1 $ cursorX st
                     , cursorY = if f2 (cursorY st) < 0
                                   then 0
                                   else f2 $ cursorY st }

