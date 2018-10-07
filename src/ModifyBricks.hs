module ModifyBricks where

import InitGame
import Graphics.Rendering.OpenGL

-- Checks if there is a collision with a brick in case of which returns true and a list of remaining bricks
checkCollision :: [Brick] -> (BallX, BallY) -> (Bool, [Brick])
checkCollision [] _ = (False , [])
checkCollision (l:ls) (x,y)
			-- check if brick at top of array is involved in collision
			|	(&&) (x - ballRadius <= xl + brickWidth
					  && y + ballRadius >= yl
					  && y <= yl + brickHeight)
					 (x + ballRadius >= xl
              		  && y + ballRadius >= yl
              		  && y <= yl + brickHeight)
              	= (True,ls)
            -- check recursively for collisions for other balls
			|	otherwise = (lt , (l: bl))
			where 
			(xl,yl,_) = l
			(lt,bl) = checkCollision ls (x,y)