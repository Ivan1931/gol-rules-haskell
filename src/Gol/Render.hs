module Gol.Render
(
    renderLoop
    , color3ify
    , ColorVec
    , SimulationState
)
where

import Gol.Rule
import Gol.Grid
import Graphics.UI.GLUT
import Data.IORef (IORef, readIORef, newIORef)

type ColorVec = Color3 GLfloat
type SimulationState = IORef (Grid, GridSize)
type ColorRule = Rule ColorVec
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

renderScene :: ColorRule -> Grid -> (Int, Int) -> IO ()
renderScene (Rule r) grid dimension =
        let
            (cellWidth, cellHeight) = dimension
            graphicalWidth = 2.0 / (fromIntegral cellWidth)
            graphicalHeight = 2.0 / (fromIntegral cellHeight)
            renderSquare' = renderSquare (graphicalWidth, graphicalHeight)
            drawCell (x, y) = color (r grid (x, y)) >> renderSquare' (graphicalX, graphicalY)
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
    (grid, dimension) <- readIORef ioState
    clear [ColorBuffer]
    renderScene rule grid dimension
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

renderLoop :: SimulationState -> ColorRule -> IO ()
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
