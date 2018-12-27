module Animation where
import Control.Monad (when)
import Data.IORef
import Data.List
import Data.Maybe
import Graphics.Rendering.OpenGL 
import Graphics.UI.GLUT 

data MoveShift = MoveShift { 
                   animVect :: Vector3 GLdouble,
                   animCurrent :: GLdouble,
                   animVectStep :: GLdouble} deriving Eq

data RotateShift = RotateShift { animCurrentAngle :: GLdouble,
                     animAngleStep :: GLdouble,
                     animAxis :: Vector3 GLdouble} deriving (Eq, Show)

data Animation = NoAnim | 
                 InfiniteAnim (Maybe MoveShift, Maybe RotateShift) |
                 FiniteAnim (Maybe MoveShift, Maybe RotateShift) deriving Eq


animStep :: IORef Animation -> IO ()
animStep shift = do
    sh <- get shift
    case sh of
        NoAnim -> return()
        InfiniteAnim (Nothing, Nothing) 
            -> shift $= NoAnim
        InfiniteAnim (Just v, Nothing) 
            -> shift $= InfiniteAnim (newvect v, Nothing)
        InfiniteAnim (Nothing, Just a)
            -> shift $= InfiniteAnim (Nothing, newangle a)
        InfiniteAnim (Just v, Just a)
            -> shift $= InfiniteAnim (newvect v, newangle a)
        FiniteAnim (Nothing, Nothing)
            -> shift $= NoAnim
        FiniteAnim (Just v, Nothing)
            -> shift $= FiniteAnim (newvect' v, Nothing)
        FiniteAnim (Nothing, Just a)
            -> do shift $= FiniteAnim (Nothing, newangle' a)
                  print a

    where
    newvect v = Just (v {animCurrent = animCurrent v + animVectStep v})
    newangle a = Just (a {animCurrentAngle = animCurrentAngle a + animAngleStep a})
    newvect' v 
        | 0 >= newcur * (animCurrent v) = Nothing
        | True = Just (v {animCurrent = newcur})
        where newcur = animCurrent v + animVectStep v
    newangle' a
        | 0 >= newa * animCurrentAngle a = Nothing
        | True = Just (a {animCurrentAngle = newa})
        where newa = animCurrentAngle a + animAngleStep a
    

applyMaybe Nothing _ = Nothing
applyMaybe (Just x) f = Just (f x)

animGetCurrent c = 
    (applyMaybe ms (\x -> (animCurrent x, animVect x)),
     applyMaybe rs (\x -> (animCurrentAngle x, animAxis x)))
    where
    (ms,rs) = case c of
                InfiniteAnim x -> x
                FiniteAnim x -> x


drawAnimated :: IORef Animation -> IO() -> IO ()
drawAnimated shift action = do
    sh <- get shift
    if sh == NoAnim
       then action
       else case animGetCurrent sh of
            (Nothing, Nothing) -> action
            (Just (c,v), Nothing) -> preservingMatrix $ do
                                     translate (mult v c)
                                     action
            (Nothing, Just (a,x)) -> preservingMatrix $ do
                                     rotate a x
                                     action
            (Just (c,v), Just (a,x)) -> preservingMatrix $ do
                                        rotate a x
                                        translate (mult v c)
                                        action

mult (Vector3 a b c) x = Vector3 (a*x) (b*x) (c*x)
scalMult (Vector3 a b c) (Vector3 a' b' c') = a*a' + b*b' + c*c'


animCallback shifts = do
    postRedisplay Nothing
    mapM_ (animStep) shifts
    addTimerCallback 40 $ animCallback shifts


