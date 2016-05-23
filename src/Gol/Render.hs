module Gol.Render 
( 
    render
    , renderHistory
    , screenDimensions
    , backgroundColor
    , colorRule
    , RenderContext
    , ColorRule
    , makeBlackBackgroundContext
)
where

import Graphics.Gloss
import Gol.Grid (get, getValues, getCells, Cell, Grid, Coord, dimensions)
import Gol.Rule (History)
import Debug.Trace

data RenderContext = RenderContext {
                        screenDimensions :: (Float, Float)
                        , backgroundColor :: Color
                        , colorRule :: ColorRule
                   }

data ColorRule = RenderByThreshold Color Cell
                | RenderByHighestValue Color
                | Other (Grid -> (Coord, Cell) -> Color)

makeBlackBackgroundContext :: (Float, Float) -> RenderContext
makeBlackBackgroundContext dims = RenderContext dims black (RenderByThreshold white 1.0)

width :: Float
width = 25.0

clamp :: Cell -> Cell -> Cell
clamp cell maxValue 
    | cell <= maxValue = cell
    | otherwise = maxValue

deriveColorFunction :: Grid -> ColorRule -> ((Coord, Cell) -> Color)
deriveColorFunction _ (RenderByThreshold base threshold) = \ (_, cell) ->
    let
        (r, g, b, a) = rgbaOfColor base
        intensity    = clamp 1.0 (cell / threshold)
    in
        makeColor (r * intensity) (g * intensity) (b * intensity) a
deriveColorFunction grid (RenderByHighestValue base) = \ (_, cell) ->
    let
        (r, g, b, a) = rgbaOfColor base
        maxCell = maximum $ getCells grid
        intensity = if maxCell <= 0.0 then 0.0 else maxCell / cell
    in
        makeColor (r * intensity) (g * intensity) (b * intensity) a
deriveColorFunction grid (Other f) = f grid
    
render :: RenderContext -> Grid -> Picture
render renderContext grid =
    let
        (screenWidth, screenHeight) = screenDimensions renderContext
        (gridWidth, gridHeight)     = dimensions $ traceShow grid grid
        blockHeight                 = screenWidth / (fromIntegral gridWidth)
        blockWidth                  = screenHeight / (fromIntegral gridHeight)
        rule                        = colorRule renderContext
        colorFunction               = deriveColorFunction grid rule
        renderCell cellInfo@((x, y), cell) = 
            color (colorFunction cellInfo) $ translate x' y' $ rectangleSolid blockWidth blockHeight
            where 
                x' = (fromIntegral x) * width
                y' = (fromIntegral y) * width
    in
        pictures $ map renderCell (getValues grid)

renderHistory :: RenderContext -> History -> Picture
renderHistory renderContext (recent:_) = render renderContext recent
