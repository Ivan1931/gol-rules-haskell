module Gol.World
(
    simulate
    , simulateWithPath
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
simulateInfinite :: Rule Cell  
        -> Grid
        -> [Grid]
simulateInfinite rule grid =
    let
        next = runRule rule grid
    in
        next : simulateInfinite rule next

readGrid :: SimulationState -> IO Grid
readGrid simState = do
    (grid, _) <- readIORef simState
    return grid

writeGrid :: SimulationState -> Grid -> IO ()
writeGrid simState nextGrid = do
    atomicModifyIORef' simState update
    where update (_, dims) = ((nextGrid, dims), ())


evolutionLoop :: SimulationState -> Rule Cell -> IO ()
evolutionLoop ref rule = do
    putStrLn "Starting evolution loop"
    (seed, _) <- readIORef ref
    mapM_ evolve $ simulateInfinite rule seed
    where 
        millis = 1000
        evolve nextGrid = do
            threadDelay (300 * millis)
            writeGrid ref nextGrid
        

simulationLoop :: Rule Cell -> Rule ColorVec -> GridSize -> Grid -> IO ()
simulationLoop evolutionRule colorRule dimensions seed = do
    ref <- newIORef (seed, dimensions)
    forkIO (renderLoop ref colorRule)
    evolutionLoop ref evolutionRule
    return ()

simulateWithPath :: Rule Cell -> Rule ColorVec -> FilePath -> IO ()
simulateWithPath cellRule colorRule path = do
    contents <- fmap lines $ readFile path
    let dims = parseGridSize $ head contents
    let grid = parseGrid dims $ (concat . tail) contents
    simulationLoop cellRule colorRule dims grid

simulate = simulationLoop
