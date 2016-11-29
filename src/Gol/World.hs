module Gol.World
(
    simulate
    , simulateWithPath
)
where

import Gol.Rule
import Gol.Grid (Grid, parse)
import Gol.Render 
import Data.IORef (IORef, newIORef, readIORef, atomicWriteIORef)
import Control.Concurrent (forkIO, threadDelay)

{-
 - Simulates a world in perpetuity
 -}
simulateInfinite :: Rule c c
        -> Grid c
        -> [Grid c]
simulateInfinite rule grid =
    let
        next = runRule rule grid
    in
        next : simulateInfinite rule next

writeGrid :: SimulationState c -> Grid c -> IO ()
writeGrid = atomicWriteIORef


evolutionLoop :: SimulationState c -> Rule c c -> IO ()
evolutionLoop ref rule = do
    putStrLn "Starting evolution loop"
    seed <- readIORef ref
    mapM_ evolve $ simulateInfinite rule seed
    where 
        millis = 1000
        evolve nextGrid = do
            threadDelay (300 * millis)
            writeGrid ref nextGrid
        

simulationLoop :: Rule c c -> Rule c ColorVec -> Grid c -> IO ()
simulationLoop evolutionRule colorRule seed = do
    ref <- newIORef seed
    forkIO (renderLoop ref colorRule)
    evolutionLoop ref evolutionRule
    return ()

simulateWithPath :: (Monoid c, Read c) => Rule c c -> Rule c ColorVec -> FilePath -> IO ()
simulateWithPath cellRule colorRule path = do
    grid <- fmap parse $ readFile path
    simulationLoop cellRule colorRule grid

simulate = simulationLoop
