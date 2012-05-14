module Input (inputCallback) where
import Graphics.Rendering.OpenGL 
import Graphics.UI.GLUT  

import Field
import Draw
import World

import System.Exit

inputCallback world key state modifiers position = 
    do gm <- get $ wGM world
       pm <- get $ wPM world
       processKey gm
       --print position
    where 
    processKey Playing = processGameKey world key state modifiers
    processKey Paused = processPauseKey world key state modifiers 
    processKey GameOver = return ()


processGameKey world (Char '\ESC') Down (Modifiers _ Up Up) = 
    System.Exit.exitWith ExitSuccess
processGameKey world (SpecialKey KeyLeft) Down (Modifiers _ Up Up) =
       processAction world $ movePiece (\[x,y,z] -> [x-1,y,z])
processGameKey world (SpecialKey KeyRight) Down (Modifiers _ Up Up) =
       processAction world $ movePiece (\[x,y,z] -> [x+1,y,z])
processGameKey world (SpecialKey KeyUp) Down (Modifiers _ Up Up) =
       processAction world $ movePiece (\[x,y,z] -> [x,y+1,z])
processGameKey world (SpecialKey KeyDown) Down (Modifiers _ Up Up) =
       processAction world $ movePiece (\[x,y,z] -> [x,y-1,z])
processGameKey world (Char ' ') Down (Modifiers _ Up Up) =
       processAction world $ dropDown 
processGameKey world (Char 'q') Down (Modifiers _ Up Up) =
       processAction world $ rotatePiece 0 0
processGameKey world (Char 'w') Down (Modifiers _ Up Up) =
       processAction world $ rotatePiece 1 0
processGameKey world (Char 'e') Down (Modifiers _ Up Up) =
       processAction world $ rotatePiece 2 0
processGameKey world (Char 'a') Down (Modifiers _ Up Up) =
       processAction world $ rotatePiece 0 1
processGameKey world (Char 's') Down (Modifiers _ Up Up) =
       processAction world $ rotatePiece 1 1
processGameKey world (Char 'd') Down (Modifiers _ Up Up) =
       processAction world $ rotatePiece 2 1
processGameKey world (Char 'p') Down (Modifiers _ Up Up) = changeGM Paused world 

processGameKey world key Down mods = print (key, mods)
processGameKey _ _ _ _  = return () 

processPauseKey world (Char 'p') Down (Modifiers _ Up Up) = changeGM Playing world 
processPauseKey _ _ _ _ = return ()

processAction world action =
     do field' <- get (wField world)
        piece' <- get (wPiece world)
        let moved = action field' piece'
        case moved of {
            Nothing -> return ();
            Just (newfield, newpiece) -> do wField world $= newfield
                                            wPiece world $= newpiece
                                            wPieceEdges world $= Nothing
                                            redraw
        }


 
