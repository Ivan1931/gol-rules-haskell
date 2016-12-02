module WireWorld where

import System.Environment
import Data.Default
import Gol
import Life (Cell(..))

red   = mkColorRGB 192 57 43
blue  = mkColorRGB 41 128 185
yellow = mkColorRGB 255 111 0
empty = mkColorRGB 38 50 56

colorWireWorld :: Rule Cell ColorVec
colorWireWorld = do
    s <- self
    return $
        case s of
            Head      -> blue
            Tail      -> red
            Conductor -> yellow
            _         -> empty

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
