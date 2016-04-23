import Gol
import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import System.Environment

background :: Color
background = black

liveColor :: Color
liveColor = white

deadColor :: Color
deadColor = black

offSetX :: Float
offSetX = 25

offSetY :: Float
offSetY = 25

cell :: Picture
cell = rectangleSolid offSetX offSetY

renderCell :: Coord -> Cell -> Picture
renderCell (x, y) c =
    let
        xCoord = offSetX * (fromIntegral x)
        yCoord = offSetY * (fromIntegral y)
        displayColor = 
            case c of
                1.0 -> liveColor
                _   -> deadColor
    in
        translate xCoord yCoord $ color displayColor cell

isAlive :: Cell -> Bool
isAlive 1.0 = True
isAlive _   = False

gameOfLife :: Rule Cell
gameOfLife = do
    s <- self
    liveCells <- countLivingAround
    if isAlive s then
        case liveCells of
            2 -> return 1.0 -- stability
            3 -> return 1.0 -- ^   ^   ^
            _ -> return 0.0 -- death by overpopulation or underpopulation
    else
        case liveCells of
            3 -> return 1.0 -- reproduction
            _ -> return 0.0
    

type World = History

renderWorld :: World -> Picture
renderWorld [] = undefined
renderWorld (recent:_) = renderGrid renderCell recent

evolve :: ViewPort -> Float -> World -> World 
evolve _ _ history = nextGrid : history
    where nextGrid = runRule gameOfLife history

main :: IO ()
main = do
    fileName <- fmap head getArgs
    fileContents <- readFile fileName
    let grid = readGrid fileContents
    simulate (InWindow "Game of Life" (1000, 1000) (20, 20)) black 3 [grid] renderWorld evolve
