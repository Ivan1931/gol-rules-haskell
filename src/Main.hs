module Main where

import Graphics.Gloss
import Gol.Grid

main :: IO ()
main = display (InWindow "Nice window" (200, 200) (10, 10)) white (Circle 80)
