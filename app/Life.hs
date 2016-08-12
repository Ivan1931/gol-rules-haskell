import Gol
import System.Environment

whiteOut :: Rule ColorVec
whiteOut = do
    s <- self
    return $ color3ify s s s

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

loadWorld :: IO Grid
loadWorld = do
    filePath <- head <$> getArgs
    putStrLn ("Read " ++ filePath)
    readGrid <$> readFile filePath


main :: IO ()
main = do
    grid <- loadWorld
    putStrLn "Starting world Simulation"
    simulateRule [grid] gameOfLife whiteOut 

