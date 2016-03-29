module Gol.Rules where

import Gol.Rule
import Gol.Grid

relativeFrom :: (X, Y) -> Rule Cell
relativeFrom (x, y) =
    Rule (\ (h:_) (x', y') -> (x' + x, y' + y) `get` h)

leftCell :: Rule Cell
leftCell = relativeFrom (-1, 0)

rightCell :: Rule Cell
rightCell = relativeFrom (1, 0)

rightAboveCell :: Rule Cell
rightAboveCell = relativeFrom (1, -1)

leftAboveCell :: Rule Cell
leftAboveCell = relativeFrom (-1, -1)

aboveCell :: Rule Cell
aboveCell = relativeFrom (0, -1)

rightBellowCell :: Rule Cell
rightBellowCell = relativeFrom (1, 1)

leftBellowCell :: Rule Cell
leftBellowCell = relativeFrom (-1, 1)

bellowCell :: Rule Cell
bellowCell = relativeFrom (0, 1)

surroundingCells :: Rule [Cell]
surroundingCells = mapM relativeFrom relativePositions
    where 
    relativePositions = [(x, y) | x <- [-1,0,1], y <- [-1, 0, 1], (x, y) /= (0, 0)] 

countLivingAround :: Rule Int
countLivingAround = 
    let
        countAlive = length . filter (==Alive)
    in 
        fmap countAlive surroundingCells
    

timeTravel :: Int -> Rule a -> Rule a
timeTravel lookback (Rule r) = 
    Rule (\ history xy -> r (drop lookback history) xy)

leftCell2Ago :: Rule Cell
leftCell2Ago = timeTravel 2 leftCell
