module GridSpec where

import Test.Hspec
import Test.QuickCheck
import Gol.Grid

cellParsingSpec = describe "Grid.cellForChar" $ do
    it "Parses an \"x\" character to Alive" $ do
        cellForChar 'x' `shouldBe` Alive
    it "Parses a \".\" character to Dead" $ do
        cellForChar '.' `shouldBe` Dead
