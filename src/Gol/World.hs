module Gol.World
(
    simulateRule
    , simulateInfinite
)
where

import Gol.Rule
import Gol.Grid
import Gol.Render 
import Data.IORef (IORef, newIORef, readIORef, atomicModifyIORef')
import Control.Concurrent (forkIO, threadDelay)

{-
 - Simulates a world in perpetuity
 -}
simulateInfinite :: History -- ^ Starting History
        -> Rule Cell  -- ^ Rule on which to run the world
        -> History
simulateInfinite history rule =
    let
        next = runRule rule history
    in
        next : simulateInfinite (next : history) rule

evolveBoundedHistory :: Rule Cell -> Int -> History -> History
evolveBoundedHistory rule bound history =
    take bound $ runRule rule history : history

evolveInfinitely :: Rule Cell -> History -> History
evolveInfinitely rule history =
    runRule rule history : history

evolutionLoop :: IORef History -> Rule Cell -> IO ()
evolutionLoop ref rule = do
    putStrLn "Starting evolution loop"
    seed <- readIORef ref
    mapM_ evolve $ simulateInfinite seed rule
    where 
        update recent history = (recent : history, ())
        millis = 1000
        evolve recent = do
            threadDelay (300 * millis)
            atomicModifyIORef' ref (update recent)
        

simulateRule :: History -> Rule Cell -> Rule ColorVec -> IO ()
simulateRule [] _ _ = error "Catastrophic failure: Initial automaton state must be provided"
simulateRule seed evolutionRule colorRule = do
    ref <- newIORef seed
    forkIO (renderLoop ref colorRule)
    evolutionLoop ref evolutionRule
    return ()

    

