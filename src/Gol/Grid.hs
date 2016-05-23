module Gol.Grid (
    get
    ,insert
    ,empty
    ,Grid
    ,Cell
    ,X
    ,Y
    ,Coord
    ,lowerBound
    ,dimensions
    ,table
    ,applyToGrid
    ,readGrid
    ,coordsFor
    ,getCoords
    ,getValues
    ,getCells
) where

import Data.Map (Map, fromList, (!), mapWithKey, keys)
import qualified Data.Map as Map
import Debug.Trace

type Cell = Float
type X = Int
type Y = Int
type Coord = (X, Y)
type Dimension = (X, Y)
type Dimensions = (X, Y)

data Grid = Grid {
                dimensions :: Dimensions
                , table :: Map Coord Cell
           } deriving (Eq, Show)

lowerBound :: Int
lowerBound = 0

empty :: Dimension -> Cell -> Grid
empty dims@(w, h) initial =
    let
        coords = coordsFor w h
        reducer map coord = Map.insert coord initial map
        table = foldl reducer Map.empty coords
    in
        Grid dims table

inBounds :: Coord -> Dimensions -> Bool
inBounds (x, y) (w, h) = x `between` (lowerBound, w) && y `between` (lowerBound, h)
    where between a (l, u) = l <= a && a <= u

createErrorMessage :: Coord -> String
createErrorMessage xy = "Unbounded coordinates " ++ (show xy)

insert :: Coord -> Cell -> Grid -> Grid
insert (x, y) c (Grid (w, h) table) =
    Grid (w, h) $ Map.insert (x', y') c table
    where 
        x' = x `mod` w
        y' = y `mod` h

get :: Coord -> Grid -> Cell
get (x, y) (Grid (w, h) table) =
    table ! (x' , y')
    where 
        x' = x `mod` w
        y' = y `mod` h

getCoords :: Grid -> [Coord]
getCoords = keys . table

getValues :: Grid -> [(Coord, Cell)]
getValues grid = zip coords $ map (`get` grid) coords
    where 
        coords = getCoords grid

getCells :: Grid -> [Cell]
getCells = (map snd) . getValues

applyToGrid :: (Coord -> Cell) -> Grid -> Grid
applyToGrid f (Grid dims table) = Grid dims nextTable
    where nextTable = mapWithKey (\ k _ -> f k) table

coordsFor :: X -> Y -> [(X, Y)]
coordsFor x y = [(i, j) | i <- [lowerBound..x-1], j <- [lowerBound..y-1]]

readGrid :: String -> Grid
readGrid strdata =
    let
        info :: [[Cell]]
        info = read strdata
        rows = length info
        columns = length $ head info
        tbl = [((c, r), info !! r !! c) | r <- [lowerBound..rows-1], c <- [lowerBound..columns-1]]
    in 
        Grid (columns, rows) $ fromList tbl
