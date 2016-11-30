module Main where

import Gol
import System.Environment
import Data.Default

data Cell = Dead | Alive
          deriving (Eq, Show, Read, Enum)

instance Default Cell where
    def = Dead

whiteOut :: Rule Cell ColorVec
whiteOut =
    let toFloat Alive = 1.0
        toFloat Dead  = 0.0
    in do
        s <- toFloat <$> self
        return $ mkColor s s s

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
    filePath <- head <$> getArgs
    simulateWithPath gameOfLife whiteOut filePath

