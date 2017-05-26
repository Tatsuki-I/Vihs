import ReadWrite
import Delete
import qualified UI.HSCurses.Curses as Curses
import qualified UI.HSCurses.CursesHelper as CursesH
import Control.Exception (finally)
import System.Posix      (usleep)

main =
  do
    Curses.initCurses
    (height, width) <- Curses.scrSize
    strbuf <- createBuffer "test.txt"
    let str = unlines $ take (height - 1) strbuf
    Curses.mvWAddStr Curses.stdScr 0 0 str
    Curses.refresh
    Curses.getCh
    let str2 = unlines $ take (height - 1) $ tail strbuf
    Curses.mvWAddStr Curses.stdScr 0 0 str2
    Curses.refresh
    Curses.getCh
    Curses.endWin

{-
main = do
    str <- readFile "test.txt"
    str <- createBuffer "test.txt"
    print str
    print $ deleteLine str 2 2
    buffToFile "test2.txt" $ deleteLine str 2 2
-}

