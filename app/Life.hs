import Gol
import System.Environment

whiteOut :: Rule ColorVec
whiteOut = do
    s <- self
    return $ color3ify s s s

gameOfLife :: Rule Cell
gameOfLife = do
    s <- self
    liveCells <- countAround (>=1.0)
    if 1.0 <= s then -- Self is alive
        case liveCells of
            2 -> return 1.0 -- stability
            3 -> return 1.0 -- ^   ^   ^
            _ -> return (dieAbit s) -- death by overpopulation or underpopulation
    else
        case liveCells of
            3 -> return 1.0 -- reproduction
            _ -> return (dieAbit s)
    where dieAbit a = 0.2 * a

loadWorld :: IO ParseResult
loadWorld = do
    filePath <- head <$> getArgs
    putStrLn ("Read " ++ filePath)
    parseGrid <$> readFile filePath

main :: IO ()
main = do
    parseResult <- loadWorld
    case parseResult of 
        Right grid ->  do
            putStrLn "Starting world Simulation"
            simulateRule [grid] gameOfLife whiteOut 
        Left deserializationError -> putStrLn $ errorMessage deserializationError

