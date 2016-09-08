module Gol.Serialization
(
    parseGrid
    , simpleParseGrid
    , errorMessage
    , DeserializationError(..)
    , ParseResult
) where

import Gol.Grid (mkGrid, lowerBound, Grid, Cell, mkGridWithList, Dimension)
import Data.List (find)
import Data.Data
import Text.ParserCombinators.Parsec

data GridSymbol = W Int -- | Width of the grid
                | H Int -- | Height of the grid
                | D [Cell] -- | Data of the board
                deriving (Eq, Show)

data DeserializationError = Inconsistent Dimension [Cell]
                          | ParsingError ParseError
                          | AmbiguousDataTypes String
                          | MissingField String
                          deriving (Eq, Show)


type ParseResult = Either DeserializationError Grid

errorMessage :: DeserializationError -> String
errorMessage (Inconsistent dimension@(width, height) symbols) = 
    unlines [
        "Error: Grid dimensions are inconsistent with number of data points",
        "(Width, Height): " ++ (show dimension),
        "Expected number of data points: " ++ (show (width * height)),
        "Number of data points: " ++ (show (length symbols))
    ]
errorMessage (ParsingError error) = show error
errorMessage (AmbiguousDataTypes fieldName) = "Error: Ambigously specified more than one " ++  fieldName
errorMessage (MissingField fieldName) = "Error: You did not include the " ++ fieldName ++ " field"

isW :: GridSymbol -> Bool
isW (W _) = True
isW _     = False

isH :: GridSymbol -> Bool
isH (H _) = True
isH _     = False


isD :: GridSymbol -> Bool
isD (D _) = True
isD _     = False

whiteSpace :: Parser Char
whiteSpace = choice [space, newline, tab]

skipWhiteSpace :: Parser ()
skipWhiteSpace = skipMany1 whiteSpace

chooseFromStrings :: [String] -> Parser String
chooseFromStrings choices = choice $ map (try . string) choices

withinBrackets :: Parser a -> Parser a
withinBrackets = between (parseWithTrailing "(") (parseWithTrailing ")")
    where parseWithTrailing s = do
            skipMany whiteSpace
            string s
            skipMany whiteSpace


widthParser :: Parser GridSymbol
widthParser = withinBrackets $ do
    chooseFromStrings [":width", ":w", "width", "w"]
    skipMany1 whiteSpace
    value <- many1 digit
    return $ W (read value :: Int)

heightParser :: Parser GridSymbol
heightParser = withinBrackets $ do
    chooseFromStrings [":height", ":h", "height", "h"]
    skipMany1 whiteSpace
    value <- many1 digit
    skipMany whiteSpace
    return $ H (read value :: Int)

numberParser :: Parser Cell
numberParser = do
        skipMany whiteSpace
        significant <- many1 digit
        char '.'
        mantissa <- many1 digit
        skipMany whiteSpace
        return (read (significant ++ "." ++ mantissa) :: Cell)

numberListParser :: Parser [Cell]
numberListParser = many1 numberParser

dataParser :: Parser GridSymbol
dataParser = withinBrackets $ do
    chooseFromStrings [":data", ":d", "data", "d"]
    skipWhiteSpace
    value <- numberListParser
    return $ D value

gridSymbolParser :: Parser GridSymbol
gridSymbolParser = do
    skipMany whiteSpace
    symbol <- try widthParser 
          <|> try heightParser 
          <|> try dataParser
    skipMany whiteSpace
    return symbol

gridParser :: Parser [GridSymbol]
gridParser = many1 gridSymbolParser >>= (\ symbols -> eof >> return symbols)

constructGrid :: [GridSymbol] -> Either DeserializationError Grid
constructGrid symbols = do
    W width <- extractWidth
    H height <- extractHeight
    D gridData <- extractData
    safelyCreateGrid (width, height) gridData
    where 
    extractGridSymbol dataTypeName dataList = 
        case dataList of
            [] ->  Left $ MissingField dataTypeName
            (x:[]) -> Right x
            _ -> Left $ AmbiguousDataTypes dataTypeName
    extractWidth = extractGridSymbol "width" $ filter isW symbols
    extractHeight = extractGridSymbol "height" $ filter isH symbols
    extractData = extractGridSymbol "data" $ filter isD symbols
    safelyCreateGrid dims@(width, height) gridData
        | width * height /= (length gridData)= Left $ Inconsistent dims gridData
        | otherwise = Right $ mkGridWithList dims gridData

parseGrid :: String -> Either DeserializationError Grid
parseGrid str =
    case parse gridParser "Game of life data parser" str of
        Left error -> Left $ ParsingError error
        Right gridSymbols -> constructGrid gridSymbols

simpleParseGrid :: String -> Grid
simpleParseGrid strdata =
    let
        info :: [[Cell]]
        info = read strdata
        rows = length info
        columns = length $ head info
        tbl = [((c, r), info !! r !! c) | r <- [lowerBound..rows-1], c <- [lowerBound..columns-1]]
    in 
        mkGrid (columns, rows) tbl
