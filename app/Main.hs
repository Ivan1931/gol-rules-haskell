import Life
import WireWorld
import Combination
import Gol
import System.Environment

runGame :: FilePath -> String -> IO ()
runGame path "gol"   = simulateWithPath gameOfLife whiteOut path
runGame path "ww"    = simulateWithPath wireWorld colorWireWorld path
runGame path "goww"  = simulateWithPath gameOfWireWorld colorGameOfWires path
runGame _ _          = error "Unknown game!"

main :: IO ()
main = do
    (game:filePath:_) <- getArgs
    runGame filePath game

