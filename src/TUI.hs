{-
import qualified UI.HSCurses.Curses       as Curses
import qualified UI.HSCurses.CursesHelper as CursesH
import Control.Exception (finally)
import System.Exit       (exitWith, ExitCode (..) )
import Control.Monad     (forever)

data Direction = L | R | D | U

move :: Direction -> IO ()
move d = do
  (height, width) <- Curses.scrSize
  (h, w) <- Curses.getYX Curses.stdScr
  Curses.wAddStr Curses.stdScr "*"
  let (h', w') = case d of
                    L -> (h, (max (w - 1) 0))
                    R -> (h, (min (w + 1) (width - 2)))
                    D -> ((min (h + 1) (height - 1)), w)
                    U -> ((max (h - 1) 0), w)
  Curses.move h' w'
  Curses.refresh

main :: IO ()
main =
  do
    CursesH.start
    Curses.echo False
    forever $ do
        Curses.refresh
        c <- CursesH.getKey (return ())
        case CursesH.displayKey c of
          "<Down>"  -> move D
          "<Up>"    -> move U
          "<Left>"  -> move L
          "<Right>" -> move R
          "j" -> move D
          "k" -> move U
          "h" -> move L
          "l" -> move R
          "q" -> exitWith ExitSuccess
          _   -> return ()
  `finally` CursesH.end
-}
import Brick

ui :: Widget ()
ui = str "Hello, world!"

main :: IO ()
main = simpleMain ui
