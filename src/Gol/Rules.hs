module Gol.Rules where

import Gol.Rule
import qualified Data.Map as Map

alive = 1.0
dead = 0.0

relativeFrom :: Coord -> Rule Cell
relativeFrom (x, y) =
    Rule (\ grid (x', y') -> (x' + x, y' + y) `getOrZero` grid)
    where getOrZero = Map.findWithDefault 0.0

neighbours :: Rule [Cell]
neighbours = mapM relativeFrom relativePositions
    where
    relativePositions = [(x, y) | x <- [-1,0,1], y <- [-1, 0, 1], (x, y) /= (0, 0)]

self :: Rule Cell
self = relativeFrom (0, 0)

neighbouringCells :: Rule [Cell]
neighbouringCells = mapM relativeFrom relativePositions
    where
    relativePositions = [(x, y) | x <- [-1,0,1], y <- [-1, 0, 1], (x, y) /= (0, 0)]

countAround :: (Cell -> Bool) -> Rule Int
countAround predicate = 
    let
        count = length . filter predicate
    in
        fmap count neighbouringCells
    
countLivingAround :: Rule Int
countLivingAround = countAround (==alive)
