module Gol.Rules where

import Gol.Rule
import Gol.Grid

alive = 1.0
dead = 0.0

relativeFrom :: (X, Y) -> Rule Cell
relativeFrom (x, y) =
    Rule (\ (h:_) (x', y') -> (x' + x, y' + y) `get` h)

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

timeTravel :: Int -> Rule a -> Rule a
timeTravel lookback (Rule r) =
    Rule (\ history xy -> r (drop lookback history) xy)
