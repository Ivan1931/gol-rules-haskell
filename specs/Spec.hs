module Main where

import GridSpec as GridSpec
import RulesSpec
import Test.QuickCheck
import Test.Hspec

main = hspec $ do
    GridSpec.spec
