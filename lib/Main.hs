import ReadWrite
import Delete

import Brick
import Brick.Widgets.Center
import Brick.Widgets.Border
import Brick.Widgets.Border.Style

ui :: String -> Widget ()
ui txt = 
    withBorderStyle unicode $
    (str txt)

main :: IO ()
main = do
    str <- readFile "test.txt"
    simpleMain (ui str)

{-
main = do
    str <- createBuffer "test.txt"
    print str
    print $ deleteLine str 2 2
    buffToFile "test2.txt" $ deleteLine str 2 2
-}
