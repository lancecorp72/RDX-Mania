--Set initial game configurations
module InitGame where

import Graphics.UI.GLUT

--Set the positions of the edges of the window
_LEFT = -2 :: GLfloat
_RIGHT = 1 :: GLfloat
_TOP = 1 :: GLfloat
_BOTTOM = -1 :: GLfloat

--Dimensions of brick, paddle and ball
brickWidth = 0.04 :: GLfloat
brickHeight = 0.25 :: GLfloat
paddleWidth = 0.01 :: GLfloat
paddleHeight = 0.35 :: GLfloat
ballRadius = 0.035 :: GLfloat

--Initial ball p=and paddle positions
_INITIAL_BALL_POSX = -0.8 :: GLfloat
_INITIAL_BALL_POSY = 0.3 :: GLfloat
_INITIAL_BALL_XDIR = -0.0015 :: GLfloat
_INITIAL_BALL_YDIR = 0.0015 :: GLfloat
_INITIAL_PADDLE_DIR = 0.005 :: GLfloat

newDx :: GLfloat->GLfloat
newDx xDir
	| xDir < 0 = xDir - 0.0005
	|otherwise = xDir + 0.0005

--Defining types Paddle and bricks
type Paddle = (GLfloat, GLfloat, GLfloat)
type Brick = (GLfloat, GLfloat, GLfloat)

--Defining Ball coordinates and the factor of movement
type BallX = GLfloat
type BallY = GLfloat
type BallDX = GLfloat
type BallDY = GLfloat

data Ball = Ball (BallX, BallY) BallDX BallDY deriving Show

--Defining game structure
data Game =
    Game { ball :: Ball
         , leftP, rightP :: Paddle
         , bricks :: [Brick]
         , points :: (Int, Int)
         , moveFactor :: GLfloat 
         }

--bricks of various types
topBricks = (-1,_TOP-brickHeight) : [ (a+brickWidth*4,b) | (a,b) <- topBricks]
botBricks = (-1,_BOTTOM+brickHeight) : [ (a+brickWidth*4,b) | (a,b) <- botBricks]
cenBricks = (-0.8,0) : [ (a+brickWidth*3.5,b) | (a,b) <- cenBricks]

--brick generator
brickPos = (take 10 topBricks) ++ (take 10 botBricks) ++ (take 5 cenBricks)

--deifining initial game configurations
initGame :: Game
initGame =
    Game { ball = Ball (_INITIAL_BALL_POSX,_INITIAL_BALL_POSY)  _INITIAL_BALL_XDIR _INITIAL_BALL_YDIR
         , leftP = (_LEFT, 0, 0)
         , rightP = (_RIGHT-paddleWidth, 0, 0)
         , bricks = initBricks   
         , points = (0,0)
         , moveFactor = 5
         }

--defining initial brick positions
initBricks :: [Brick]
initBricks = map (\(a,b) -> (a,b,0) ) brickPos
