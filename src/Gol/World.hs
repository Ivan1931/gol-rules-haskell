module Gol.World
(
    simulateFinite
    , simulateInfinite
    , simulateRule
)
where

import Gol.Rule
import Gol.Grid
import Gol.Render 
import Graphics.Gloss (simulate)
import Graphics.Gloss (Display (InWindow))
import Graphics.Gloss.Data.ViewPort (ViewPort)

{-
 - Simulates a world in perpetuity
 -}
simulateInfinite :: Grid -- ^ Seed grid
        -> Rule Cell  -- ^ Rule on which to run the world
        -> History
simulateInfinite seed rule =
        foldl step [seed] [1..]
    where 
        step history _ = runRule rule history : history

simulateFinite :: Int -- ^ Number of states to produce
           -> Grid -- ^ Starting grid
           -> Rule Cell -- ^ Game Rule to apply to grid
           -> History
simulateFinite i g r = take i $ simulateInfinite g r

simulateWithWindow :: Int -- ^ size of the window
               -> Grid -- ^ Starting grid
               -> Rule Cell -- ^ Game Rule
               -> History -- ^ An array the size of the starting window
simulateWithWindow i seed rule =
        foldl step [seed] [1..]
    where
        step history _ = runRule rule history : take i history


evolveBoundedHistory :: Rule Cell -> Int -> History -> History
evolveBoundedHistory rule bound history =
    take bound $ runRule rule history : history

evolveInfinitely :: Rule Cell -> History -> History
evolveInfinitely rule history =
    runRule rule history : history

simulateRule :: RenderContext -> History -> Rule Cell -> IO ()
simulateRule context seed rule =
    let
        background = backgroundColor context
        (w, h)     = screenDimensions context
        dims       = (round w, round h)
        origin     = (0, 0)
        evolve     = evolveInfinitely rule
    in
        simulate (InWindow "Game of Life" dims origin) 
                 background
                 3
                 seed
                 (renderHistory context)
                 (\_ _ history -> evolve history)

