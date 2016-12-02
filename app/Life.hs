module Life where

import Gol
import System.Environment
import Data.Default

data Cell = Alive | Empty | Head | Tail | Conductor
          deriving (Eq, Show, Read, Enum)

instance Default Cell where
    def = Empty

colorGameOfLife :: Rule Cell ColorVec
colorGameOfLife = do
    s <- self
    return $ case s of
        Alive -> mkColorRGB 102 187 106
        _     -> mkColorRGB 38 50 56

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
