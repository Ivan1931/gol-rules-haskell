module Gol.Render
(
    renderLoop
    , color3ify
    , ColorVec
    , SimulationState
)
where

import Gol.Rule
import Gol.Grid (Grid(..), coordsFor, Coord, GridSize)
import Graphics.UI.GLUT
import Data.IORef (IORef, readIORef, newIORef)

type ColorVec = Color3 GLfloat
type SimulationState c = IORef (Grid c)
type ColorRule c = Rule c ColorVec
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

renderScene :: ColorRule c -> Grid c -> IO ()
renderScene (Rule r) grid@(Grid size table) =
        let
            (cellWidth, cellHeight) = size
            graphicalWidth = 2.0 / (fromIntegral cellWidth)
            graphicalHeight = 2.0 / (fromIntegral cellHeight)
            renderSquare' = renderSquare (graphicalWidth, graphicalHeight)
            drawCell (x, y) = color (r grid (x, y)) >> renderSquare' (graphicalX, graphicalY)
                where graphicalX = (fromIntegral x) * graphicalWidth - 1.0
                      graphicalY = (fromIntegral y) * graphicalHeight - 1.0
        in
            mapM_ drawCell $ coordsFor size

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

unmarshalAndRender :: ColorRule c -> SimulationState c -> IO ()
unmarshalAndRender rule ioState = do
    grid <- readIORef ioState
    clear [ColorBuffer]
    renderScene rule grid
    flush

reshape :: ReshapeCallback
reshape size = do
  viewport $= (Position 0 0, size)

keyboardMouse :: KeyboardMouseCallback
keyboardMouse _key _state _modifiers _position = return ()

display :: ColorRule c -> SimulationState c -> DisplayCallback
display rule ioState = unmarshalAndRender rule ioState

idle :: ColorRule c -> SimulationState c -> IdleCallback
idle rule state = unmarshalAndRender rule state

renderLoop :: SimulationState c -> ColorRule c -> IO ()
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
