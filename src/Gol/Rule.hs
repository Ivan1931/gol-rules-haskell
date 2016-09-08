module Gol.Rule where

import Gol.Grid (Grid, X, Y, Coord, applyToGrid, Cell)

{-|
 - A rule is a function that takes a history of grids and a cell position and
 - associates them with another value
 -}

type History = [Grid]

newtype Rule a = Rule { rule :: History -> Coord -> a }

{-|
 - Intent: The functor here applies a function in the context of a history and cell position.
 -         The Rule Functor  captures the context of the history.
 - Does it obey the functor laws?
 - # fmap id = id
 -      fmap id (Rule r) =
 -          Rule (\ history xy -> id $ r history xy) 
 -      = Rule r ==> fmap id = id
 -# fmap g . fmap f $ Rule r 
 -          = fmap g (fmap f (Rule r))
 -          = fmap g $ Rule (\ history xy -> f $ r history xy)
 -          = Rule (\ history' xy' -> g $ (\history xy -> f $ r history xy) history' xy')
 -          = Rule (\ history xy -> (g . f) $ r history xy)
 -          = fmap (g . f) (Rule r)
 -
 -}
instance Functor Rule where
    fmap f (Rule r) = Rule (\ history xy -> f $ r history xy)

{-|
 - Does it obey the Applicative laws?
 - "pure id <*> v = v"
 - pure id <*> Rule r
 -      = Rule (\ _ _ -> id) <*> Rule r
 -      = Rule (\ history xy -> ((\ _ _ -> id) history xy) $ r history xy)
 -      = Rule (\ history xy -> id $ r history xy)
 -      = Rule r
 - "pure f <*> pure x = pure (f x)"
 - pure f <*> pure x
 -      = Rule (\ _ _ -> f) <*> Rule (\ _ _ -> x)
 -      = Rule (\ history xy -> ((\ _ _ -> f) history xy) $ (\ _ _ -> x) history xy)
 -      = Rule (\ history xy -> f $ x)
 -      = Rule (\ history xy -> f x)
 -      = pure (f x)
 - "u <*> pure y = pure ($y) <*> u"
 - u <*> pure y 
 -      = Rule f <*> Rule (\ _ _ -> y)
 -      = Rule (\ history xy -> (f history xy) $ (\ _ _ -> y) history xy
 -      = Rule (\ history xy -> (f history xy) $ y)
 -      = Rule (\ history xy -> ($y) (f history xy))
 -      = Rule (\ history xy -> ((\ _ _ -> ($y) history xy) $ (f history xy)))
 -      = pure ($y) <*> u
 - "u <*> v <*> w) = pure (.) <*> u <*> v <*> w
 -}

instance Applicative Rule where
    pure a = Rule (\_ _ -> a)
    Rule f <*> Rule r =
        Rule (\ history xy -> (f history xy) $ r history xy)

{-
 -  "return a >>= f === f a"
 -  "m >>= return === m"
 -  "(m >>= k) >>= h === m >>= (\ x -> k x >>= h)"
 -}
instance Monad Rule where
    return = pure
    Rule r >>= f = Rule $ \ history xy ->
        let
            a = r history xy
            Rule f' = f a
        in
            f' history xy


runRule :: Rule Cell -> History -> Grid
runRule _ [] = error "History must be non-empty"
runRule (Rule r) history@(recent:_) = applyToGrid ruleWithHistory recent
    where ruleWithHistory = r history
