module Life where

import Gol
import System.Environment
import Data.Default

data Cell = Alive | Empty | Head | Tail | Conductor
          deriving (Eq, Show, Read, Enum)

instance Default Cell where
    def = Empty

colorGameOfLife :: Rule Cell ColorVec
colorGameOfLife =
    let toFloat Alive = 1.0
        toFloat _     = 0.0
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
            _ -> return def
    else
        case liveCells of
            3 -> return Alive
            _ -> return def
