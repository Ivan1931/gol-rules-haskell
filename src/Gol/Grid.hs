module Gol.Grid (
    get
    ,insert
    ,empty
    ,Grid
    ,Cell
    ,X
    ,Y
    ,Coord
) where

import Data.Map (Map, fromList, (!))
import qualified Data.Map as Map

type Cell = Double
type X = Int
type Y = Int
type Coord = (X, Y)
type Dimension = (X, Y)
type Dimensions = (X, Y)

data Grid = Grid {
                dimensions :: Dimensions
                , grid :: Map Coord Cell
           } deriving (Eq, Show)

lowerBound :: Int
lowerBound = 1

empty :: Dimension -> Cell -> Grid
empty dims@(w, h) initial =
    let
        coords = [(i, j) | i <- [lowerBound..w], j <- [lowerBound..h]]
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
get xy (Grid dims table) =
    if xy `inBounds`  dims then
        table ! xy
    else
        error $ createErrorMessage xy
