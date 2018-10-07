module Display where

import Graphics.UI.GLUT  (swapBuffers, postRedisplay, addTimerCallback, Timeout, leaveMainLoop)
import Graphics.Rendering.OpenGL
import Graphics.UI.GLUT.Fonts
import Data.IORef
import InitGame
import Gameplay
import Shapes

-- Displays a given rectangle with given width and height
displayItem :: (GLfloat, GLfloat, GLfloat) -> GLfloat -> GLfloat -> String -> IO ()
displayItem (x,y,_) width height typ = preservingMatrix $ do
    translate $ Vector3 (width/2) (height/2) 0
    displayAt (x,y) $ rectangle width height typ

-- Displays all bricks present at current time instance
displayBricks :: [Brick] -> IO ()
displayBricks [] = leaveMainLoop
displayBricks (x:[]) = displayItem x brickWidth brickHeight "br"
displayBricks (x:xs) = do
			displayItem x brickWidth brickHeight "br"
			displayBricks xs

-- Displays the given string in the window
displayString:: String -> IO()
displayString s = preservingMatrix $
	do
	currentColor $= Color4 0.1 0.8 1 1
	scale 0.0013 0.0015 (0::GLfloat)
	renderString Roman s

-- Function which is called by others to display objects at given position
displayAt :: (GLfloat, GLfloat) -> IO a -> IO a
displayAt (x,y) displayMe = preservingMatrix $ do
    translate $ Vector3 x y (0::GLfloat)
    displayMe

-- Driver function for displaying all Window Items continuously
display :: IORef Game -> IO()
display game = do
    clear[ColorBuffer]
    g <- get game
    let (Ball pos xDir yDir) = ball g
    displayString "RDX Mania"
    displayAt pos $ fillCircle ballRadius "bl"
    displayItem (leftP g) paddleWidth paddleHeight "p"
    displayItem (rightP g) paddleWidth paddleHeight "p"
    displayBricks (bricks g)
    swapBuffers

-- Called periodically from Main module to update the Game actions
timer :: IORef Game -> IO ()
timer game = do
    addTimerCallback frameRate $ timer game
    g <- get game
    let fac = moveFactor g
        (bl , br ) = moveBall g
    game
        $= g{ ball = bl
            , bricks = br
            , leftP = movePaddle (leftP g) fac
            , rightP = movePaddle (rightP g) fac
            }
    postRedisplay Nothing

-- Sets the frame rate
frameRate :: Timeout
frameRate = 1000 `div` 60

-- Maintains the Window
reshape :: t -> Size -> IO ()
reshape game s@(Size w h) = do
    viewport $= (Position 0 0, s)
    matrixMode $= Projection
    loadIdentity
    ortho (-2.0) 1.0 (-1.0) 1.0 (-1.0) 1.0
    matrixMode $= Modelview 0

