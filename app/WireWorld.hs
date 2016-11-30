module Main where

import System.Environment
import Data.Default
import Gol

data Wire = Empty | Head | Tail | Conductor
          deriving (Eq, Show, Read, Enum)

instance Default Wire where
    def = Empty

red   = mkColor 0.8 0.1 0.1
green = mkColor 0.1 0.8 0.1
blue  = mkColor 0.1 0.1 0.8
yellow = mkColor 1.0 0.8 0.02

colorWireWorld :: Rule Wire ColorVec
colorWireWorld = do
    s <- self
    return $
        case s of
            Empty     -> green
            Head      -> blue
            Tail      -> red
            Conductor -> yellow

wireWorld :: Rule Wire Wire
wireWorld = do
    s <- self
    n <- countAround (==Head)
    return $ case s of
        Conductor ->
            if n == 2 || n == 1 then
                Head
            else
                Conductor
        Empty    -> Empty
        _        -> succ s

main = do
    filePath <- head <$> getArgs
    simulateWithPath wireWorld colorWireWorld filePath
