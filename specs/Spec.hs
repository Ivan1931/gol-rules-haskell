module Main where

import GridSpec
import RulesSpec
import Test.QuickCheck
import Test.Hspec

main = hspec $ do
    cellParsingSpec
