module Gol.Render 
( 
    renderGrid
)
where

import Graphics.Gloss
import Gol.Grid

renderGrid :: (Coord -> Cell -> Picture) -> Grid -> Picture
renderGrid renderCell grid =
    let
        (w, h) = dimensions grid
        coords = coordsFor w h
        coordsWithCells = zip coords $ map (`get` grid) coords
    in
        pictures $ map (uncurry renderCell) coordsWithCells
