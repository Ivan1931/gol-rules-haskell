import Gol
import System.Environment

whiteOut :: Rule ColorVec
whiteOut = do
    s <- self
    let t = realToFrac s
    return $ color3ify t t t

gameOfLife :: Rule Cell
gameOfLife = do
    s <- self
    liveCells <- countAround (>=1.0)
    if 1.0 <= s then
        case liveCells of
            2 -> return 1.0
            3 -> return 1.0
            _ -> return 0.0
    else
        case liveCells of
            3 -> return 1.0
            _ -> return 0.0

main :: IO ()
main = do
    filePath <- fmap head getArgs
    simulateWithPath gameOfLife whiteOut filePath

