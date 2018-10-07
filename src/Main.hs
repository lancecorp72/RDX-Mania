-- Main program which integrates all files
module Main where

import Display
import InitGame
import Bindings
import Graphics.UI.GLUT as GLUT
import Data.IORef


main :: IO ()
main = do
    --initialize OpenGL interface and creating a window
    (progName, _) <- getArgsAndInitialize
    initialDisplayMode $= [GLUT.DoubleBuffered]
    createWindow progName
    game <- newIORef initGame
    fullScreen
    displayCallback $= display game

    --Setting up the game object
    --This part is looped over till the user exits
    keyboardMouseCallback $= Just (keyboard game)
    reshapeCallback $= Just(reshape game)
    addTimerCallback frameRate $ timer game
    mainLoop
