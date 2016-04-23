module Main where

import GridSpec
import RulesSpec
import Test.QuickCheck
import Test.Hspec

main = hspec $ do
    describe "The truth" $ do
        it "Is true" $ do 
            True `shouldBe` True
