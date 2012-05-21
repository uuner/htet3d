module World where
import Control.Monad (when)
import Data.IORef
import Data.List
import Data.Maybe
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT
import qualified Data.Map as M

import Field
import Animation
import Colors

data World = World {
                wWidth :: Int,
                wHeight :: Int,
                wDepth :: Int,
                wField :: IORef Field,
                wPiece :: IORef Piece,
                wAnimNext :: IORef Animation,
                wColoredLayers :: Bool,
                wPieceEdges :: IORef (Maybe [([GLdouble], [GLdouble])]),
                wPM :: IORef ProgramMode,
                wGM :: IORef GameMode,
                wAnimate :: IORef Bool,
                wCallCount :: IORef Int
             }

data GameMode = Playing | Paused | GameOver deriving Eq
data ProgramMode = MainMode | MenuMode | HelpMode deriving Eq

changeGM mode world = do
    wGM world $= mode
    field' <- get $ wField world
    case mode of
        Playing -> setPieceFallCallback world
        _ -> return ()
    postRedisplay Nothing

changePM mode world = do
    wPM world $= mode
    gm <- get $ wGM world
    when (gm == Playing) (setPieceFallCallback world)
    postRedisplay Nothing

setPieceFallCallback world = do
    field <- get $ wField world
    calls <- get (wCallCount world)
    wCallCount world $= calls + 1
    addTimerCallback (levelTime ! level field)
                     (pieceFallCallback world)

pieceFallCallback world = do
    calls <- get (wCallCount world)
    pm <- get (wPM world)
    gm <- get (wGM world)

    if calls > 1 || pm /= MainMode || gm /= Playing
       then
         wCallCount world $= calls - 1
       else
         do field' <- get (wField world)
            piece' <- get (wPiece world)
            postRedisplay Nothing
            if not $ isOver field'
                then do (newfield, newpiece) <- dropPiece field' piece'
                        wField world $= newfield
                        wPiece world $= newpiece
                        wPieceEdges world $= Nothing
                        addTimerCallback (levelTime ! level field')
                                         (pieceFallCallback world)
                else wGM world $= GameOver

initWorld = do
      let width  = 5
          heigth = 5
          depth  = 10
      rPiece <- createRandomPiece width heigth depth
      field <- newIORef (createField width heigth depth rPiece)
      rPiece <- createRandomPiece width heigth depth
      piece <- newIORef rPiece
      edges <- newIORef Nothing
      nextAnimation <- newIORef (InfiniteAnim (Nothing,
          Just RotateShift { animCurrentAngle = 0,
                             animAngleStep = 0.5,
                             animAxis = Vector3 0 0 (fromIntegral depth :: GLdouble)}))
      pieceAnim <- newIORef Nothing
      pm <- newIORef MainMode
      gm <- newIORef Playing
      animate <- newIORef False
      calls <- newIORef 1
      return World { wWidth = width,
                     wHeight = heigth,
                     wDepth = depth,
                     wField = field,
                     wPiece = piece,
                     wAnimNext = nextAnimation,
                     wColoredLayers = True,
                     wPieceEdges = edges,
                     wPM = pm,
                     wGM = gm,
                     wCallCount = calls,
                     wAnimate = animate }
