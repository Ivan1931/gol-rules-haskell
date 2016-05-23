import Gol
import System.Environment

gameOfLife :: Rule Cell
gameOfLife = do
    s <- self
    liveCells <- countLivingAround
    if s == 1.0 then -- Self is alive
        case liveCells of
            2 -> return 1.0 -- stability
            3 -> return 1.0 -- ^   ^   ^
            _ -> return 0.0 -- death by overpopulation or underpopulation
    else
        case liveCells of
            3 -> return 1.0 -- reproduction
            _ -> return 0.0 
    
main :: IO ()
main = do
    fileName <- fmap head getArgs
    fileContents <- readFile fileName
    let grid = readGrid fileContents
    let context = makeBlackBackgroundContext (700, 700)
    simulateRule context [grid] gameOfLife
