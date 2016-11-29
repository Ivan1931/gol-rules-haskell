module Gol.Grid where

import Data.Map (fromList, union, Map)
import Gol.Rule

type GridSize = (Int, Int)

lowerBound :: Int
lowerBound = 0

coordsFor :: Int -> Int -> [Coord]
coordsFor x y = [(i, j) | i <- [lowerBound..x-1], j <- [lowerBound..y-1]]

parseGridSize :: String -> GridSize
parseGridSize line =
    let
        size@(width, height) = read line :: (Int, Int)
    in
        if width < 0 then error "Negative width specified"
        else if height < 0 then error "Negative height specified"
        else size

parseGrid :: GridSize -> String -> Grid
parseGrid (width, height) mapRep = 
    let
        specified = read mapRep :: Map Coord Cell
        unspecified = fromList $ coordsFor width height `zip` repeat 0.0
    in
        specified `union` unspecified


parse :: [String] -> Grid
parse (x:xs) =
    let
        dims = parseGridSize x
    in
        parseGrid dims $ concat xs
