module Gol.Grid (
    get
    ,mkGrid
    ,mkGridWithLists
    ,mkGridWithList
    ,insert
    ,empty
    ,Grid
    ,Cell
    ,X
    ,Y
    ,Coord
    ,Dimension
    ,lowerBound
    ,dimension
    ,table
    ,applyToGrid
    ,coordsFor
    ,getCoords
    ,getValues
    ,getCells
) where

import Data.Map (Map, fromList, (!), mapWithKey, keys)
import qualified Data.Map as Map
type Cell = Float
type X = Int
type Y = Int
type Coord = (X, Y)
type Dimension = (X, Y)

data Grid = Grid {
                dimension :: Dimension
                , table :: Map Coord Cell
           } deriving (Eq, Show)

lowerBound :: Int
lowerBound = 0

mkGrid :: Dimension -> [(Coord, Cell)] -> Grid
mkGrid dims table = Grid dims $ fromList table

mkGridWithLists :: Dimension -> [[Cell]] -> Grid
mkGridWithLists dims@(width, height) cellLists =
    let
        listsWithIndex = zip [lowerBound..] cellLists
        indexNumbers y = map (\ x -> (x, y)) [lowerBound..]
        addIndexToNumbers (y, xs) = indexNumbers y `zip` xs 
        table = concatMap addIndexToNumbers listsWithIndex
    in
        mkGrid dims table

mkGridWithList :: Dimension -> [Cell] -> Grid
mkGridWithList dims@(width, height) cells = mkGridWithLists dims $ chunk width cells
    where 
    chunk _ [] = []
    chunk i cells = take i cells : (chunk i (drop i cells))

empty :: Dimension -> Cell -> Grid
empty dims@(w, h) initial =
    let
        table = zip (coordsFor w h) (repeat initial)
    in
        mkGrid dims table

inBounds :: Coord -> Dimension -> Bool
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
