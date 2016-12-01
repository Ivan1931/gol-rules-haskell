module Combination where

import Gol
import Life
import WireWorld
import Data.Default
import Control.Monad (liftM2)

comeAlive :: Rule Cell Cell
comeAlive = do
    s <- self
    n <- countAround (==Head)
    return $
        if s == def && n == 1 then
            Alive
        else
            def
            
gameOfWireWorld :: Rule Cell Cell
gameOfWireWorld = liftM2 pick gameOfLife wireWorld
    where pick Alive Empty     = Alive
          pick Empty Alive     = Empty
          pick _     a         = a

gameOfWireWorldComingAlive :: Rule Cell Cell
gameOfWireWorldComingAlive = liftM2 pick gameOfWireWorld comeAlive
    where pick Empty Alive = Alive
          pick a     _     = a

colorGameOfWires :: Rule Cell ColorVec
colorGameOfWires = do
    s <- self
    if s == Alive then
        colorGameOfLife
    else
        colorWireWorld
