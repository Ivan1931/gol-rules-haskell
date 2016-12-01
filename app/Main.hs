import Life
import WireWorld
import Combination
import Gol
import System.Environment

runGame :: FilePath -> String -> IO ()
runGame path "gol"   = simulateWithPath gameOfLife colorGameOfLife path
runGame path "ww"    = simulateWithPath wireWorld colorWireWorld path
runGame path "goww"  = simulateWithPath gameOfWireWorld colorGameOfWires path
runGame path "gowwc" = simulateWithPath gameOfWireWorldComingAlive colorGameOfWires path
runGame _ _          = error "Error: Unknown game! \n\
                             \ Possible games are:\n\
                             \ gol - Game of Life\n\
                             \ ww  - Wire world \n\
                             \ goww - combination of game of life and wireworld \n\
                             \ gowwc - game of life combined with wireworld and some secret sauce\n"

main :: IO ()
main = do
    (game:filePath:_) <- getArgs
    runGame filePath game

