{-# LANGUAGE ScopedTypeVariables #-}

module Gol.Grid (
    Grid(..)
    , Coord
    , GridSize
    , parseWithDefault
    , parse
    , mkGrid
    , coordsFor
) where

import Data.Map (fromList, union, Map)
import Data.Default

type GridSize = (Int, Int)
type Coord = (Int, Int)

data Grid c = Grid { 
                 size :: GridSize, 
                 table :: Map Coord c
            } deriving(Eq, Show, Read)


lowerBound :: Int
lowerBound = 0

coordsFor :: (Int, Int) -> [Coord]
coordsFor (x, y) = [(i, j) | i <- [lowerBound..x-1], j <- [lowerBound..y-1]]

parseWithDefault :: (Read c) => c -> String -> Grid c
parseWithDefault def xs =
    let
        (Grid size table) = read xs
        unspecified = fromList $ coordsFor size `zip` repeat def
    in
        mkGrid size $ table `union` unspecified

mkGrid :: (Int, Int) -> Map Coord c -> Grid c
mkGrid size@(width, height) table
    | width < 0 = error "Width is negative"
    | height < 0 = error "Height is too small"
    | otherwise = Grid size table

parse :: (Read c, Default c) => String -> Grid c
parse = parseWithDefault def
