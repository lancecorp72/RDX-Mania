--Basic physics of the game
module Gameplay where

import InitGame
import ModifyBricks
import Graphics.UI.GLUT

--Function to move the ball
moveBall ::Game -> (Ball, [Brick])
moveBall g
    = (Ball (x+factor*newXDir, y+factor*newYDir) newXDir newYDir , newBricks)
       
      where
	--position of the paddles
        (xl,yl,_) = leftP g 
        (xr,yr,_) = rightP g

        factor = moveFactor g
      
        (Ball (x,y) xDir yDir) = ball g

	--Check if ball collides with any of the bricks        
	(collided, newBricks) = checkCollision (bricks g) (x,y)
	--collided becomes true if the ball collides with a brick and newBricks is the list of bricks
	--excluding the collided one        
        
        newXDir
	    --reverse direction if ball hits a brick
            |   collided
              = - newDx xDir
            --reverse direction if ball hits the left paddle
            |    x - ballRadius <= xl + paddleWidth
              && y + ballRadius >= yl
              && y              <= yl + paddleHeight
              = -xDir
            --reverse direction if ball hits the right paddle
            |    x + ballRadius >= xr
              && y + ballRadius >= yr
              && y              <= yr + paddleHeight
              = -xDir
            --stop the ball if it falls off the screen
            | x <= _LEFT - ballRadius = 0
            | x >= _RIGHT + ballRadius = 0
            | otherwise = xDir
        newYDir
            --bounce off the top and bottom walls
            | y <= _BOTTOM - (ballRadius/4) || y >= _TOP + (ballRadius/4) = -yDir
            
	    --if x becomes 0, then stop movement along y as well.
            | newXDir == 0 = 0
            | otherwise = yDir

--function to move the paddle
movePaddle :: Paddle -> GLfloat -> Paddle
movePaddle p@(x,y,dir) factor =
    let y1 = y + factor * dir
    	newY :: GLfloat
	--stop the paddle if it hits the top and bottom of the screen
        newY = min (_TOP-paddleHeight) $ max _BOTTOM y1
    in (x, newY, dir)
