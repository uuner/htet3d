{-# LANGUAGE NoMonomorphismRestriction #-}
module Draw where
import Control.Monad (when)
import Data.IORef
import Data.List
import Data.Maybe
import Graphics.Rendering.OpenGL 
import Graphics.UI.GLUT

import Field
import Animation
import Colors
import World

import qualified Data.Map as M

display world = preservingMatrix $ do
    (Size winW winH) <- get windowSize
    field <- get $ wField world
    piece <- get $ wPiece world
    pm <- get $ wPM world
    gm <- get $ wGM world
    animate <- get $ wAnimate world

    clear [ColorBuffer, DepthBuffer]
    depthFunc $= Just Lequal

    let sqsize = fromIntegral (min winH (div (3*winW) 4)) :: GLsizei
    let nextsize = div (fromIntegral sqsize) 3 :: GLsizei

    --Main gameboard
    viewport $= (Position 0 (fromIntegral $ winH - sqsize), Size sqsize sqsize)
    drawFrame ""
    if gm == Paused
        then
        drawPause
        else do
             drawPlaygnd
             drawField field (wColoredLayers world)
             if (not $ isOver field) 
                then (drawPiece piece (wPieceEdges world))
                else overMessage

    --Next piece
    viewport $= (Position (fromIntegral sqsize) (fromIntegral $ winH - nextsize), Size nextsize nextsize)
    drawFrame "Next"

    if gm == Playing && pm == MainMode && animate == True
        then 
        drawNext (nextPiece field) (Just $ wAnimNext world)
        else
        drawNext (nextPiece field) Nothing

    viewport $= (Position (fromIntegral sqsize) (fromIntegral $ winH - nextsize - (div nextsize 4)), 
                 Size nextsize (div nextsize 4))
    drawFrame $ "Score: " ++ (show $ scores field)
    viewport $= (Position (fromIntegral sqsize) (fromIntegral $ winH - nextsize - (div nextsize 2)), 
                 Size nextsize (div nextsize 4))
    drawFrame $ "Level: " ++ (show $ level field)
    
    {-viewport $= (Position sqsize 0, Size nextsize nextsize)
    drawFrame ""
    printString $ show $ (center piece, boxes piece)-}

    swapBuffers
    where
    drawPause = do
        color green1
        renderObject Wireframe (Teapot 5)
    --    renderPrimitive 

    drawPlaygnd = do
        color green1
        let x = fromIntegral (wWidth world)/2 :: GLdouble
            y = fromIntegral (wHeight world)/2
            z = fromIntegral (wDepth world) 
        renderPrimitive Lines $ do
            mapM_ (\xx -> (vertex $ Vertex3 xx (-y) 0) >> (vertex $ Vertex3 xx (-y) z)) [0-x,1-x..x]
            mapM_ (\xx -> (vertex $ Vertex3 xx y 0) >> (vertex $ Vertex3 xx y z)) [0-x,1-x..x]
            mapM_ (\xx -> (vertex $ Vertex3 xx y 0) >> (vertex $ Vertex3 xx (-y) 0)) [0-x,1-x..x]
            mapM_ (\yy -> (vertex $ Vertex3 x yy 0) >> (vertex $ Vertex3 x yy z)) [0-y,1-y..y]
            mapM_ (\yy -> (vertex $ Vertex3 (-x) yy 0) >> (vertex $ Vertex3 (-x) yy z)) [0-y,1-y..y]
            mapM_ (\yy -> (vertex $ Vertex3 (-x) yy 0) >> (vertex $ Vertex3 x yy 0)) [0-y,1-y..y]
        mapM_ (\zz -> renderPrimitive LineLoop $ do 
                              vertex $ Vertex3 (-x) (-y) zz
                              vertex $ Vertex3 x (-y) zz
                              vertex $ Vertex3 x y zz
                              vertex $ Vertex3 (-x) y zz
                              ) [0,1..z]  
    drawBox x y z col mode = do
                {-d <- get depthMask
                print $ "depth " ++ show d
                b <- get blend
                print b-}
                let xx = fromIntegral (wWidth world)/2 :: GLdouble
                    yy = fromIntegral (wHeight world)/2
                    zz = fromIntegral (wDepth world) 
                    vect = Vector3 (fromIntegral x - xx + 1/2) 
                                   (fromIntegral y - yy + 1/2) 
                                   (fromIntegral z + 1/2)
                color col
                if mode == 2 
                    then (
                    do blend $= Enabled
                       depthMask $= Disabled
                       cube2 vect
                       blend $= Disabled
                       depthMask $= Enabled)
                    else if mode == 1 then cube2 vect else return ()
                cube vect
    drawField field@(Field {glass = gl}) True = 
        mapM_ (\([x,y,z], c) -> drawBox x y z ((cycle $ layerCols)!!z) 1) $ M.toList gl
    drawField field@(Field {glass = gl}) False = mapM_ (\([x,y,z], c) -> drawBox x y z c 1) $ M.toList gl
    layerCols = [green1, 
                 green6, 
                 blue4,
                 blue6,
                 blue7,
                 violet,
                 pink,
                 red,
                 white]
    drawPiece' (Piece {boxes = bs, pcol = col}) =
        mapM_ (\[x,y,z] -> drawBox x y z col 2) bs

    drawNext :: Piece -> (Maybe (IORef Animation)) -> IO()
    drawNext piece Nothing = do
        preservingMatrix $ do
            translate (Vector3 0 0 ((0)::GLdouble))
            drawPiece' piece

    drawNext piece (Just nextAnimation) = do
        preservingMatrix $ do
            translate (Vector3 0 0 ((0)::GLdouble))
            drawAnimated nextAnimation $ drawPiece' piece
        
    drawPiece piece edges = do
        drawPieceWireframe piece edges
        drawPieceBody piece
    drawPieceWireframe (Piece {boxes = bs}) edges = do
                edges' <- get edges
                when (isNothing edges')
                     (do let eds = genEdges bs
                         edges $= Just eds)
                edges' <- get edges
                let eds = fromJust edges'
                let xx = fromIntegral (wWidth world)/2 :: GLdouble
                    yy = fromIntegral (wHeight world)/2
                    zz = fromIntegral (wDepth world)
                    vect = Vector3 (0 - xx) (0 - yy) 0
                color white
                preservingMatrix $
                    do translate vect
                       oldLW <- get lineWidth
                       lineWidth $= 2
                       renderPrimitive Lines $ do
                             mapM_ lineVertex eds
                             vertex $ Vertex3 0 0 (0::GLdouble) 
                       lineWidth $= oldLW
                       
    drawPieceBody (Piece {boxes = bs, pcol = col}) = do
                let xx = fromIntegral (wWidth world)/2 :: GLdouble
                    yy = fromIntegral (wHeight world)/2
                    zz = fromIntegral (wDepth world) 
                    vect = Vector3 (0 - xx) (0 - yy) 0
                color col
                blend $= Enabled
                depthMask $= Disabled
                preservingMatrix $
                    do translate vect
                       renderPrimitive Quads $ do
                             mapM_ (\[x,y,z] -> vertex $ Vertex3 x y (z::GLdouble)) $ 
                                concat $
                                map head $ 
                                filter (\x -> length x == 1) $ 
                                group $ 
                                sort $ 
                                concatMap sides bs
                blend $= Disabled
                depthMask $= Enabled

    lineVertex ([x,y,z],[x',y',z']) = 
        do vertex $ Vertex3 x y (z::GLdouble)
           vertex $ Vertex3 x' y' (z'::GLdouble)

genEdges :: [[Int]] -> [([GLdouble],[GLdouble])]
genEdges bs = [ s | n <- [0..2], l <- layers n, s <- sticks n l]
    where
    layers n = map (\xs@(x:_) -> (x!!n, map (cut n) xs)) $
               groupBy (\x y -> x!!n == y!!n) $
               sortBy (\x y -> compare (x!!n) (y!!n)) bs
    sticks n (z, xs) = map head $ filter (\x -> not $ length x `elem` [2,4]) $ group $ sort
                [(map fromIntegral (ins n z [x',y']), map fromIntegral (ins n (z+1) [x',y'])) 
                                    | [x,y] <- xs, x' <- [x,x+1], y' <- [y,y+1]]

sides coord' = [ map (ins i z') [[x',y'],[x'+1,y'],[x'+1,y'+1],[x',y'+1]] |
                    let coord = map (\x -> fromIntegral x) coord',
                    i <- [0..2], 
                    let [x',y'] = cut i coord, 
                    z' <- [coord!!i, (coord!!i) + 1]]

cut n s = take n s ++ drop (n+1) s
ins n x s = take n s ++ x:(drop (n) s)

cube v = preservingMatrix $ 
            do translate v
               color black
               renderObject Wireframe (Cube $ 1)
cube2 v = preservingMatrix $ 
            do translate v
               let w = 1/2::GLdouble
               renderPrimitive Quads $ do
                   mapM_ (\(c, xs) ->
                                 mapM_ (\(a,b,c) -> vertex $ Vertex3 (w*a) (w*b) (w*c)) xs) $ 
                                 zip [0.6,0.7..] cubeBase

cubeBase = [zip3 (c!!x1) (c!!x2) (c!!x3) | 
                let a = [[1,1,1,1], [1,1,-1,-1], [1,-1,-1,1]],
                [x1,x2,x3] <- [[0,1,2], [1,0,2], [1,2,0]]::[[Int]], 
                c <- [a, (map negate $ head a):(tail a)]]

overMessage = do
    matrixMode $= Projection 
    preservingMatrix $ do
        loadIdentity
        ortho2D (-10.0) 10.0 (-10.0) 10.0
        matrixMode $= Modelview 0 
        preservingMatrix $ do
            loadIdentity
            color (Color3 1.0 0.0 0.0:: Color3 GLfloat)
            rasterPos (Vertex2 (0-1) (0) :: Vertex2 GLint)
            renderString Helvetica18 "Game Over"
    matrixMode $= Modelview 0

drawFrame text = do
    matrixMode $= Projection 
    preservingMatrix $ do
        loadIdentity
        ortho2D (-10.1) 10.1 (-10.1) 10.1
        matrixMode $= Modelview 0 
        preservingMatrix $ do
            loadIdentity
            color (Color3 0.8 0.8 0.8:: Color3 GLfloat)
            oldLW <- get lineWidth
            lineWidth $= 3
            renderPrimitive LineLoop $ do
                vertex $ Vertex2 (-10) ((-10)::GLdouble)
                vertex $ Vertex2 (-10) (10::GLdouble)
                vertex $ Vertex2 10 (10::GLdouble)
                vertex $ Vertex2 10 ((-10)::GLdouble)
            lineWidth $= oldLW
            rasterPos (Vertex2 (-9) (-8) :: Vertex2 GLint)
            renderString Helvetica18 text
    matrixMode $= Modelview 0

printString s = do
   color (Color3 1.0 1.0 0.0:: Color3 GLfloat)
   rasterPos (Vertex2 (-8) (-8) :: Vertex2 GLint)
   renderString Helvetica18 s

redraw = postRedisplay Nothing

