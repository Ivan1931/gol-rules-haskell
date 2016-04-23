module Gol.Rules where

import Gol.Rule
import Gol.Grid

alive = 1.0
dead = 1.0

relativeFrom :: (X, Y) -> Rule Cell
relativeFrom (x, y) =
    Rule (\ (h:_) (x', y') -> (x' + x, y' + y) `get` h)

surroundingCells :: Rule [Cell]
surroundingCells = mapM relativeFrom relativePositions
    where
    relativePositions = [(x, y) | x <- [-1,0,1], y <- [-1, 0, 1], (x, y) /= (0, 0)]

countLivingAround :: Rule Int
countLivingAround =
    let
        countAlive = length . filter (==alive)
    in
        fmap countAlive surroundingCells

timeTravel :: Int -> Rule a -> Rule a
timeTravel lookback (Rule r) =
    Rule (\ history xy -> r (drop lookback history) xy)
