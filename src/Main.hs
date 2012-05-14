import Control.Monad 
import Data.IORef
import Data.Maybe
import Graphics.Rendering.OpenGL 
import Graphics.UI.GLUT 
import System.Time    

import Draw
import Input
import Field
import Animation
import World

main = do world <- initWorld
          getArgsAndInitialize
          initialDisplayMode $= [DoubleBuffered, WithDepthBuffer, RGBMode ]
          let w = 800::GLsizei
              h = 600::GLsizei
          initialWindowSize $= Size w h
          createWindow "Tetris3D"

          let viewAngle = 60 :: GLdouble
          let viewDist = 0.1 + fromIntegral (wDepth world) + 
                (fromIntegral (max (wWidth world) (wHeight world)) / 2) / (tan $ viewAngle * pi / 360)
          perspective viewAngle 1 1 100
          lookAt  (Vertex3 0 0 (viewDist ::GLdouble)) 
                  (Vertex3 0 0 (0::GLdouble)) 
                  (Vector3 0 1 (0::GLdouble))

          depthFunc $= Just Lequal
          blend $= Enabled
          blendFunc $= (SrcAlpha, OneMinusSrcAlpha) 
          displayCallback $= display world
          keyboardMouseCallback $= Just (inputCallback world)
          addTimerCallback 100 $ pieceFallCallback world
          addTimerCallback 100 $ animCallback [wAnimNext world]
          mainLoop


