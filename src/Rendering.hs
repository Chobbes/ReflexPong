module Rendering where

import Constants
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color


renderPong :: PongState -> Picture
renderPong (PongState ball playerPaddle enemyPaddle) = 
  Pictures [playerPic, ballPic, enemyPic, wallPic]

  where playerPic = drawSolidBox playerColour playerPaddle
        enemyPic = drawSolidBox enemyColour enemyPaddle
        ballPic = drawBall ballColour ball
        wallPic = drawWireBox wallColour wallBox


drawSolidBox :: Color -> Box -> Picture
drawSolidBox c (Box (x,y) w h) = translate x y (color c (rectangleSolid w h))


drawWireBox :: Color -> Box -> Picture
drawWireBox c (Box (x,y) w h) = translate x y (color c (rectangleWire w h))


drawBall :: Color -> Ball -> Picture
drawBall c (Ball (x,y) _) = translate x y (color c (circle ballRadius))
