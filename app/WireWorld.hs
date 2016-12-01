module WireWorld where

import System.Environment
import Data.Default
import Gol
import Life (Cell(..))

red   = mkColor 0.8 0.1 0.1
green = mkColor 0.1 0.8 0.1
blue  = mkColor 0.1 0.1 0.8
yellow = mkColor 1.0 0.8 0.02

colorWireWorld :: Rule Cell ColorVec
colorWireWorld = do
    s <- self
    return $
        case s of
            Empty     -> green
            Head      -> blue
            Tail      -> red
            Conductor -> yellow

wireWorld :: Rule Cell Cell
wireWorld = do
    s <- self
    n <- countAround (==Head)
    return $ case s of
        Conductor ->
            if n == 1 || n == 2 then
                Head
            else
                Conductor
        Empty    -> Empty
        Head     -> Tail
        Tail     -> Conductor
        a        -> a
