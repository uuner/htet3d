module Colors where
import Graphics.Rendering.OpenGL

type PieceColor = Color4 GLdouble

red,red1,red2,red3 :: Color4 GLdouble
red     = Color4 1.0  0.0  0.0 0.5
red1    = Color4 0.75 0.0 0.0 0.5
red2    = Color4 0.5  0.0  0.0 0.5
red3    = Color4 0.25 0.0 0.0 0.5

pink,yellow,orange :: Color4 GLdouble
pink    = Color4 1.0  0.5  0.5 0.5
yellow  = Color4 1.0  1.0  0.0 0.5
orange  = Color4 1.0  0.5  0.0 0.5

green,green1,green2,green3,green4,green5,green6 :: Color4 GLdouble
green   = Color4 0.0  1.0  0.0 0.5
green1  = Color4 0.0  0.75 0.0 0.5
green2  = Color4 0.0  0.5  0.0 0.5
green3  = Color4 0.0  0.25 0.0 0.5
green4  = Color4 0.0  1.0 0.25 0.5
green5  = Color4 0.0  1.0 0.25 0.5
green6  = Color4 0.0  1.0 0.5  0.5

blue,blue1,blue2,blue3,blue4,blue5,blue6,blue7:: Color4 GLdouble
blue    =  Color4 0.0 0.0 1.0  0.5
blue1   =  Color4 0.0 0.0 0.75 0.5
blue2   =  Color4 0.0 0.0 0.5  0.5
blue3   =  Color4 0.0 0.0 0.25 0.5
blue4   =  Color4 0.0 0.75 0.5  0.5
blue5   =  Color4 0.0 0.5 0.75 0.5
blue6   =  Color4 0.0 0.25 1.0 0.5
blue7   =  Color4 0.5 0.0 1.0 0.5

violet,white,black :: Color4 GLdouble
violet  = Color4 (1.0::GLdouble)  0.0  1.0 0.5
white   = Color4 (1.0::GLdouble)  1.0  1.0 0.5
black   = Color4 (0.0::GLdouble)  0.0  0.0 0.5
