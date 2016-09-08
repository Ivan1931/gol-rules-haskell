module SerializationSpec (spec) where

import Test.Hspec
import Gol.Serialization
import Gol.Grid

isParseError :: Either DeserializationError Grid -> Bool
isParseError (Left (ParsingError _)) = True
isParseError _ = False

parseGridSpec = do
    describe "Gol.Serialization.parseGrid" $ do
        it "parses correctly formatted (3, 3) grid" $ do
            let testGrid = mkGridWithLists (3,3) [[0.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 0.0]]
            let testGridRepresentation = unlines [
                                            "(width 3)", "(height 3)",
                                            "(data 0.0 0.0 0.0",
                                            "      0.0 1.0 0.0",
                                            "      0.0 0.0 0.0)"
                                            ]
            parseGrid testGridRepresentation `shouldBe` Right testGrid

        it "parses the correct dimensions" $ do
            let testGrid = mkGrid (3, 2) [((0, 0), 1), ((1, 0), 0), ((2, 0), 0),
                                          ((0, 1), 0), ((1, 1), 1), ((2, 1), 0)]
            let testGridRepresentation = unlines [
                                            "(w 3) (h 2)",
                                            "(data 1.0 0.0 0.0",
                                            "      0.0 1.0 0.0)"
                                         ]
            parseGrid testGridRepresentation `shouldBe` Right testGrid

        it "returns missing field error when field is missing" $ do
            let missingError = MissingField "data"
            let missingFieldGrid = "(width 3) (height 3)"
            parseGrid missingFieldGrid `shouldBe` Left missingError

        it "returns Inconsistent with inconsistent dimensions" $ do
            let inconsistentDimensionsError = Inconsistent (1,2) [1.0]
            let inconsistentDimensions = "(w 1) (h 2) (d 1.0)"
            parseGrid inconsistentDimensions `shouldBe` Left inconsistentDimensionsError

        it "returns AmbiguousDataTypes error when there is more than one data field inputted" $ do
            let ambigousDataTypesError = AmbiguousDataTypes "width"
            let ambigousDataTypesGrid = "(w 1) (:width 2) (height 2) (data 1.0 1.0)"
            parseGrid ambigousDataTypesGrid `shouldBe` Left ambigousDataTypesError

        it "returns ParsingError for missing bracket" $ do
            let missingBracket = "(w 1 (h 2) (d 1.0 1.0)"
            parseGrid missingBracket `shouldSatisfy` isParseError

        it "returns ParsingError for an integer formatted float" $ do
            let floatWidth = "(w 1.0) (h 2) (d 1.0 1.0)"
            parseGrid floatWidth `shouldSatisfy` isParseError

        it "returns ParsingError for an integer data field" $ do
            let integerData = "(w 1) (h 2) (d 1 1)"
            parseGrid integerData `shouldSatisfy` isParseError

        it "returns a ParsingError for more than one integer field in a width field" $ do
            let moreThanOneInteger = "(w 1 1) (h 1 1) (d 1.0 1.2)"
            parseGrid moreThanOneInteger `shouldSatisfy` isParseError

spec = parseGridSpec
