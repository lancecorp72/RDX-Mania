module Rendering where

import Graphics.UI.GLUT
import Graphics.Rendering.OpenGL

-- function which associates the given points with colours depending on the object type
displayPoints :: [(GLfloat, GLfloat, GLfloat)] -> String -> PrimitiveMode -> IO ()
displayPoints points "p" primitiveShape = do
	currentColor $= Color4 1 0 0 1
	displayMyPoints points primitiveShape

displayPoints points "br" primitiveShape = do
	currentColor $= Color4 0 1 0 1
	displayMyPoints points primitiveShape

displayPoints points "bl" primitiveShape = do
	currentColor $= Color4 1 1 1 1
	displayMyPoints points primitiveShape

displayPoints points _ primitiveShape = do
	currentColor $= Color4 0 0 1 1
	displayMyPoints points primitiveShape

-- function which displays the given shape at required coordinate
displayMyPoints :: [(GLfloat, GLfloat, GLfloat)] -> PrimitiveMode -> IO ()
displayMyPoints points primitiveShape = do
    renderAs primitiveShape points
    flush

-- function which renders the given shape on screen
renderAs :: PrimitiveMode -> [(GLfloat, GLfloat, GLfloat)] -> IO ()
renderAs figure ps = renderPrimitive figure $ makeVertices ps

-- function to convert a 3-tuple to a valid point in this coordinate system
makeVertices :: [(GLfloat, GLfloat, GLfloat)] -> IO ()
makeVertices = mapM_ (\(x,y,z) -> vertex $ Vertex3 x y z)
