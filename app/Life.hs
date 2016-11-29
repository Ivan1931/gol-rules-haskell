import Gol
import System.Environment

data Cell = Alive | Dead
          deriving (Eq, Show, Read)

instance Monoid Cell where
    mempty = Dead
    mappend Dead Alive = Alive
    mappend Alive Dead = Alive
    mappend Alive Alive = Dead

toFloat :: Cell -> Float
toFloat Alive = 1.0
toFloat Dead = 0.0

whiteOut :: Rule Cell ColorVec
whiteOut = do
    s <- self
    let t = toFloat s
    return $ color3ify t t t

gameOfLife :: Rule Cell Cell
gameOfLife = do
    s <- self
    liveCells <- countAround (==Alive)
    if s == Alive then
        case liveCells of
            2 -> return Alive
            3 -> return Alive
            _ -> return Dead
    else
        case liveCells of
            3 -> return Alive
            _ -> return Dead

main :: IO ()
main = do
    filePath <- fmap head getArgs
    simulateWithPath gameOfLife whiteOut filePath

