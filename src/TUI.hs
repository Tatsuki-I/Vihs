--module TUI where

import Graphics.Vty

main =  do vty <- standardIOConfig >>= mkVty
           update vty . picForImage $ string (defAttr `withForeColor` green) "Hello vty"
           e <- nextEvent vty
           shutdown vty
           print e

