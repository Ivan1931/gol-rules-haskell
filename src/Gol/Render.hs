module Gol.Render
(
    renderLoop
    , color3ify
    , ColorVec
)
where

import Gol.Grid (get, getValues, getCells, Cell, Grid, Coord, dimension)
import Gol.Rule
import Gol.Grid (coordsFor)
import Graphics.UI.GLUT
import Data.IORef (IORef, readIORef)

type ColorVec = Color3 GLfloat
type ColorRule = Rule ColorVec
type SimulationState = IORef History
type Location = (GLfloat, GLfloat)
type Dimension = (GLfloat, GLfloat)

renderSquare :: Dimension -> Location -> IO ()
renderSquare (w, h) (x, y) =
    renderPrimitive Quads $ do
        vertex $ Vertex2 leftX topY
        vertex $ Vertex2 rightX topY
        vertex $ Vertex2 rightX bottomY
        vertex $ Vertex2 leftX bottomY
    where
         leftX = x
         rightX = x + w
         topY = y
         bottomY = y + h

renderScene :: ColorRule -> History -> IO ()
renderScene (Rule r) history@(mostRecent:_) =
    let
        (cellWidth, cellHeight) = dimension mostRecent
        graphicalWidth = 2.0 / (fromIntegral cellWidth)
        graphicalHeight = 2.0 / (fromIntegral cellHeight)
        renderSquare' = renderSquare (graphicalWidth, graphicalHeight)
        drawCell (x, y) = color (r history (x, y)) >> renderSquare' (graphicalX, graphicalY)
            where graphicalX = (fromIntegral x) * graphicalWidth - 1.0
                  graphicalY = (fromIntegral y) * graphicalHeight - 1.0
    in
        mapM_ drawCell $ coordsFor cellWidth cellHeight

cube :: GLfloat -> IO ()
cube w = do
  renderPrimitive Quads $ do
    vertex $ Vertex3 w w w
    vertex $ Vertex3 w w (-w)
    vertex $ Vertex3 w (-w) (-w)
    vertex $ Vertex3 w (-w) w
    vertex $ Vertex3 w w w
    vertex $ Vertex3 w w (-w)
    vertex $ Vertex3 (-w) w (-w)
    vertex $ Vertex3 (-w) w w
    vertex $ Vertex3 w w w
    vertex $ Vertex3 w (-w) w
    vertex $ Vertex3 (-w) (-w) w
    vertex $ Vertex3 (-w) w w
    vertex $ Vertex3 (-w) w w
    vertex $ Vertex3 (-w) w (-w)
    vertex $ Vertex3 (-w) (-w) (-w)
    vertex $ Vertex3 (-w) (-w) w
    vertex $ Vertex3 w (-w) w
    vertex $ Vertex3 w (-w) (-w)
    vertex $ Vertex3 (-w) (-w) (-w)
    vertex $ Vertex3 (-w) (-w) w
    vertex $ Vertex3 w w (-w)
    vertex $ Vertex3 w (-w) (-w)
    vertex $ Vertex3 (-w) (-w) (-w)
    vertex $ Vertex3 (-w) w (-w)

unmarshalAndRender :: ColorRule -> SimulationState -> IO ()
unmarshalAndRender rule ioState = do
    state <- readIORef ioState
    clear [ColorBuffer]
    renderScene rule state
    flush

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)

keyboardMouse :: KeyboardMouseCallback
keyboardMouse _key _state _modifiers _position = return ()

display :: ColorRule -> SimulationState -> DisplayCallback
display rule ioState = unmarshalAndRender rule ioState

idle :: ColorRule -> SimulationState -> IdleCallback
idle rule state = unmarshalAndRender rule state

renderLoop :: IORef History -> Rule ColorVec -> IO ()
renderLoop ref rule = do
  putStrLn "Starting render loop"
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Cellular Automaton Generator"
  displayCallback $= display rule ref
  idleCallback $= Just (idle rule ref)
  reshapeCallback $= Just reshape
  keyboardMouseCallback $= Just keyboardMouse
  mainLoop

color3ify :: Float -> Float -> Float -> ColorVec
color3ify r g b = Color3 r g b
