module GridSpec (spec) where

import Test.Hspec
import Gol.Grid

sampleDim = 3

sampleBoard :: Grid
sampleBoard = foldl step (empty (sampleDim, sampleDim) 0.0) coords
    where
        checkSum x y = (fromIntegral x) * 13.0 + (fromIntegral y) * 13.0
        step grid (x, y) = insert (x, y) (checkSum x y) grid
        coords = coordsFor sampleDim sampleDim



boundsReachAroundSpec = do
    describe "Gol.Grid.get" $ do
        it "(-1, 0) should describe right column top element" $ do
            (-1, 0) `get` sampleBoard `shouldBe` (sampleDim - 1, 0) `get` sampleBoard
        it "(0, -1) should describe bottom row left element" $ do
            (0, -1) `get` sampleBoard `shouldBe` (0, sampleDim - 1) `get` sampleBoard
        it "(3, 3) refers to top right hand corner (0, 0)" $ do
            (3, 3) `get` sampleBoard `shouldBe` (0, 0) `get` sampleBoard

otherSpec = do
    describe "The truth" $ do
        it "is always true" $ do
            True `shouldBe` True

spec = boundsReachAroundSpec >> otherSpec
