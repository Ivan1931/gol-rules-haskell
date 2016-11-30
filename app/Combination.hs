module Combination where

import Gol
import Life
import WireWorld

comeAlive :: Rule Cell Cell
comeAlive = do
    s <- self
    heads <- countAround (==Head)
    return $
        if 2 <= heads then
            Alive
        else
            s
            
    
-- A head wire set's an empty sell to alive
-- But kills a living cell
gameOfWireWorld :: Rule Cell Cell
gameOfWireWorld = do
    w <- wireWorld
    g <- gameOfLife
    c <- comeAlive
    return $ 
        case (c, g, w) of
            (Alive, _, Empty) -> Alive
            (_, Alive, Empty) -> Alive
            _                 -> w

colorGameOfWires :: Rule Cell ColorVec
colorGameOfWires = do
    s <- self
    if s == Alive then
        whiteOut
    else
        colorWireWorld
