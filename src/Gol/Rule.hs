module Gol.Rule where

import Data.Map (Map, mapWithKey)

type Coord = (Int, Int)
type Cell = Double
type Grid = Map Coord Cell

{-|
 - A rule is a function that takes a history of grids and a cell position and
 - associates them with another value
 -}

newtype Rule a = Rule { rule :: Grid -> Coord -> a }

{-|
 - Intent: The functor here applies a function in the context of a grid and cell position.
 -         The Rule Functor  captures the context of the grid.
 - Does it obey the functor laws?
 - # fmap id = id
 -      fmap id (Rule r) =
 -          Rule (\ grid xy -> id $ r grid xy) 
 -      = Rule r ==> fmap id = id
 -# fmap g . fmap f $ Rule r 
 -          = fmap g (fmap f (Rule r))
 -          = fmap g $ Rule (\ grid xy -> f $ r grid xy)
 -          = Rule (\ grid' xy' -> g $ (\grid xy -> f $ r grid xy) grid' xy')
 -          = Rule (\ grid xy -> (g . f) $ r grid xy)
 -          = fmap (g . f) (Rule r)
 -
 -}
instance Functor Rule where
    fmap f (Rule r) = Rule (\ grid xy -> f $ r grid xy)

{-|
 - Does it obey the Applicative laws?
 - "pure id <*> v = v"
 - pure id <*> Rule r
 -      = Rule (\ _ _ -> id) <*> Rule r
 -      = Rule (\ grid xy -> ((\ _ _ -> id) grid xy) $ r grid xy)
 -      = Rule (\ grid xy -> id $ r grid xy)
 -      = Rule r
 - "pure f <*> pure x = pure (f x)"
 - pure f <*> pure x
 -      = Rule (\ _ _ -> f) <*> Rule (\ _ _ -> x)
 -      = Rule (\ grid xy -> ((\ _ _ -> f) grid xy) $ (\ _ _ -> x) grid xy)
 -      = Rule (\ grid xy -> f $ x)
 -      = Rule (\ grid xy -> f x)
 -      = pure (f x)
 - "u <*> pure y = pure ($y) <*> u"
 - u <*> pure y 
 -      = Rule f <*> Rule (\ _ _ -> y)
 -      = Rule (\ grid xy -> (f grid xy) $ (\ _ _ -> y) grid xy
 -      = Rule (\ grid xy -> (f grid xy) $ y)
 -      = Rule (\ grid xy -> ($y) (f grid xy))
 -      = Rule (\ grid xy -> ((\ _ _ -> ($y) grid xy) $ (f grid xy)))
 -      = pure ($y) <*> u
 - "u <*> v <*> w) = pure (.) <*> u <*> v <*> w
 -}

instance Applicative Rule where
    pure a = Rule (\_ _ -> a)
    Rule f <*> Rule r =
        Rule (\ grid xy -> (f grid xy) $ r grid xy)

{-
 -  "return a >>= f === f a"
 -  "m >>= return === m"
 -  "(m >>= k) >>= h === m >>= (\ x -> k x >>= h)"
 -}
instance Monad Rule where
    return = pure
    Rule r >>= f = Rule $ \ grid xy ->
        let
            a = r grid xy
            Rule f' = f a
        in
            f' grid xy


runRule :: Rule Cell -> Grid -> Grid
runRule (Rule r) grid = mapWithKey f grid
    where f xy _ = r grid xy
