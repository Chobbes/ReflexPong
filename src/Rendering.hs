module Rendering where

import Constants
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color


renderPong :: PongState -> Picture
renderPong (PongState (Ball (bx,by) _) py ey) = Pictures [playerPaddle, ball, enemyPaddle, walls]
  where playerPaddle = drawPaddle playerColour playerX py paddleWidth paddleHeight
        enemyPaddle = drawPaddle enemyColour enemyX ey paddleWidth paddleHeight
        ball = color ballColour (translate bx by (circle ballRadius))
        walls = color wallColour (rectangleWire (wallRight - wallLeft) (wallTop - wallBottom))


drawPaddle :: Color -> Float -> Float -> Float -> Float -> Picture
drawPaddle c x y w h = translate x y (color c (rectangleSolid w h))
