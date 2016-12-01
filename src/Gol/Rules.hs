module Gol.Rules where

import Gol.Rule
import Gol.Grid (Coord, Grid(..))
import Data.Default
import Data.Map (findWithDefault)

relativeFrom :: (Default a) => Coord -> Rule a a
relativeFrom (x, y) = Rule $ \ (Grid (width, height) table) (x', y') -> 
    let
        getOrZero = findWithDefault def
        x'' = (x + x') `mod` width
        y'' = (y + y') `mod` height
    in
        (x'', y'') `getOrZero` table

neighbours :: (Default a) => Rule a [a]
neighbours = mapM relativeFrom relativePositions
    where
    relativePositions = [(x, y) | x <- [-1,0,1], 
                                  y <- [-1,0,1], 
                                  (x, y) /= (0, 0)]

self :: (Default a) => Rule a a
self = relativeFrom (0, 0)

countAround :: (Default a) => (a -> Bool) -> Rule a Int
countAround predicate = 
    let
        count = length . filter predicate
    in
        fmap count neighbours
