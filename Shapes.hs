module Shapes where

import Rendering
import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

-- function to display a rectangle of given dimensions
rectangle :: GLfloat -> GLfloat -> String -> IO()
rectangle length width typ =
    displayPoints [(l,w,0), (l,-w,0),(-l,-w,0),(-l,w,0)] typ Quads
    where
        l = length / 2
        w = width / 2

-- function to display a circle with given radius and scale
circlePoints :: GLfloat -> GLfloat -> [(GLfloat, GLfloat, GLfloat)]
circlePoints radius number
    = [let alpha = (2*pi) * i / number
       in (radius * sin alpha, radius * cos alpha, 0)
      | i <- [1,2..number]]

-- function to display a circle of given radius
circle :: GLfloat -> [(GLfloat, GLfloat, GLfloat)]
circle radius = circlePoints radius 100

-- function which renders the circle on window
renderCircle :: GLfloat -> String -> IO ()
renderCircle r typ = displayPoints (circle r) typ LineLoop

-- function which fills the interior of circle
fillCircle :: GLfloat -> String -> IO ()
fillCircle r typ = displayPoints (circle r) typ Polygon
