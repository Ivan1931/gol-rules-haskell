module Gol.Grid where

import Data.Map (Map, fromList, (!))
import qualified Data.Map as Map

data Cell = Alive | Dead
          deriving(Eq, Show, Read)

type X = Int
type Y = Int
type Line = String
type Row = [Cell]
type Coord = (X, Y)

data Grid = Grid {
                width :: X
                , height :: Y
                , grid :: Map Coord Cell
           } deriving (Eq, Show)

lowerBound :: Int
lowerBound = 1

empty :: X -> Y -> Grid
empty x y = Grid x y Map.empty

inBounds :: (X, Y) -> (X, Y) -> Bool
inBounds (x, y) (w, h) = 
    x `inBounds'` (lowerBound, w) && y `inBounds'` (lowerBound, h)
    where
        inBounds' v (low, high) = low <= v && v <= high

insert :: Coord -> Cell -> Grid -> Grid
insert xy cell (Grid w h table)
    | inBounds xy (w, h) = 
        Grid w h $ Map.insert xy cell table
    | otherwise = 
        error (show xy ++ " is out of bounds with " ++ show (w, h))

parseChar :: Char -> Cell
parseChar 'x' = Alive
parseChar '.' = Dead
parseChar _   = error "Unrecognised cell type!"

{-
 - Apply line takes a line of dots "." and x's "x" and applies each lines to it's proper row column
 - position in a grid
 -}
parseLine :: Line -> Row
parseLine = map parseChar
 
{-|The 'applyRow' function takes 'row' of cells and a 'Y' axis coordinate
 - places the cells in sequence from 1 to until length of the list
 -}
applyRow :: Grid -> Y -> [Cell] -> Grid
applyRow grid y row =
    let 
        insertSquare' grid (x, cell) = insert (x, y) cell grid
    in
        foldl insertSquare' grid $ zip [1..] row
    
{-
 We serialise a grid very simply
 First line is two numbers representing the x and y coordinates of the grid
 The following lines are eather full stops "." representing an empty cell or an "x" representing
 a live cell
 Example:
 2 2
 .x
 x.

 Another example:
 3 4
 xxx
 x..
 ..x
 ...

 -}
parseGrid :: String -> Grid
parseGrid str =
    let 
        (first:rest) = lines str
        indexedLines = zip [lowerBound..] rest
        wrapper grid (idx, line) = applyRow grid idx $ parseLine line
        [x, y] = map (read :: String -> Int) $ words first
        emptyGrid = empty x y
    in
        foldl wrapper emptyGrid indexedLines

get :: Coord -> Grid -> Cell
get xy (Grid w h table)
    | inBounds xy (w, h) = table ! xy
    | otherwise =
        error $ "Gol.Grid.get - coordinates " 
            ++  (show xy) 
            ++ " are out of bounds "
            ++  (show (w, h))
