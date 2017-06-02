import ReadWrite
import Delete
import qualified UI.HSCurses.Curses as Curses
import qualified UI.HSCurses.CursesHelper as CursesH
import Control.Exception (finally)
import System.Posix      (usleep)
import System.Exit       (exitWith, ExitCode (..) )
import Control.Monad     (forever)

screenBuffer :: [String] -> Int -> String
screenBuffer strbuf height = unlines $ take (height - 1) strbuf

{-
main =
  do
    Curses.initCurses
    (height, width) <- Curses.scrSize
    strbuf <- createBuffer "test.txt"
    Curses.mvWAddStr Curses.stdScr 0 0 $ screenBuffer strbuf height
    Curses.refresh
    Curses.getCh
    Curses.mvWAddStr Curses.stdScr 0 0 $ screenBuffer (tail strbuf) height
    Curses.refresh
    Curses.getCh
    Curses.endWin
-}

{-
main = do
    str <- readFile "test.txt"
    str <- createBuffer "test.txt"
    print str
    print $ deleteLine str 2 2
    buffToFile "test2.txt" $ deleteLine str 2 2
-}

