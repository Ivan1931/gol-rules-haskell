module Gol.Render 
( 
    render
)
where

import Gol.Grid (get, getValues, getCells, Cell, Grid, Coord, dimensions)
import Gol.Rule

type Picture = String

-- Each of our things produces a renderable cube
render :: Rule Picture -> History -> IO ()
render (Rule r) history = undefined
