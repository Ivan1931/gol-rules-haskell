module Gol.Rule where

import Gol.Grid
import Data.Map (mapWithKey)

{-|
 - A rule is a function that takes a history of grids and a cell position and
 - associates them with another value
 -}

newtype Rule g a = Rule { rule :: Grid g -> Coord -> a }

instance Functor (Rule g) where
    fmap f (Rule r) = Rule (\ grid xy -> f $ r grid xy)

instance Applicative (Rule g) where
    pure a = Rule (\_ _ -> a)
    Rule f <*> Rule r =
        Rule (\ grid xy -> (f grid xy) $ r grid xy)

instance Monad (Rule g) where
    return = pure
    Rule r >>= f = Rule $ \ grid xy ->
        let
            a = r grid xy
            Rule f' = f a
        in
            f' grid xy

runRule :: Rule g a -> Grid g -> Grid a
runRule (Rule r) grid@(Grid size table) = 
    let 
        f xy _ = r grid xy
    in
        mkGrid size $ mapWithKey f table
