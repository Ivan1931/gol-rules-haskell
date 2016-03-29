{-|We base our testing code on the premise that
 - two functions are most probably equal if we
 - feed them hundereds of the same random examples and they both
 - produce the same outputs
 - 
 - We use multiple rules to test our the equality factors. Our tests
 - are thus a mixture of manual unit tests and QuickCheck randomised
 - property based tests
 -}
module RulesSpec where

import Gol.Rules
import Gol.Grid
import Test.Hspec
import Test.QuickCheck

