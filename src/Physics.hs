module Physics where

import Constants
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Pure.Game
import Control.Applicative
import Debug.Trace


pongSimulate :: Point -> Float -> PongState -> PongState
pongSimulate (_,my) step state = state { ballState = newBall
                                       , playerY = my
                                       , enemyY = snd . ballPos $ newBall
                                       }
  where movedBall = advanceBall (ballState state)
        advanceBall (Ball (px,py) (vx,vy)) = Ball (px + step * vx, py + step * vy) (vx,vy)
        collision = ballCollisions state movedBall
        newBall = case collision of
                    Nothing -> movedBall
                    Just refAxis -> movedBall {ballVel = reflect refAxis (ballVel movedBall)}


reflect :: Vector -> Vector -> Vector
reflect axis v = v `subV` ((2 * (dotV axis v)) `mulSV` axis)
  where subV (x1,y1) (x2,y2) = (x1 - x2, y1 - y2)


-- | If there is a collision, return the normal of the collision.
ballCollisions :: PongState -> Ball -> Maybe Vector
ballCollisions state ball = wallCollide <|> enemyCollide <|> playerCollide
  where playerCollide = ballPaddleCollide playerX (playerY state) (1,1) (1,-1) ball
        enemyCollide = ballPaddleCollide enemyX (enemyY state) (-1,1) (-1,-1) ball
        wallCollide = ballWallsCollide ball


ballPaddleCollide :: Float -> Float -> Vector -> Vector -> Ball -> Maybe Vector
ballPaddleCollide x y topV bottomV (Ball (ballX,ballY) _) =
                  if inYRange && inXRange
                     then Just paddleNorm
                     else Nothing
  where inYRange = ballY - ballRadius <= y + h && ballY + ballRadius >= y - h
        inXRange = ballX - ballRadius <= x + w && ballX + ballRadius >= x - w

        paddleNorm = normalizeV (tx + bx, ty + by)
          where relY = (ballY - y + h) / paddleHeight
                (tx, ty) = relY `mulSV` topV
                (bx, by) = (1 - relY) `mulSV` bottomV

        w = paddleWidth / 2
        h = paddleHeight / 2


ballWallsCollide :: Ball -> Maybe Vector
ballWallsCollide (Ball (x,y) _) = topCol <|> bottomCol <|> leftCol <|> rightCol
  where topCol = if y + ballRadius >= wallTop
                    then Just (0,-1)
                    else Nothing
        bottomCol = if y - ballRadius <= wallBottom
                       then Just (0,1)
                       else Nothing
        leftCol = if x - ballRadius <= wallLeft
                     then Just (1,0)
                     else Nothing
        rightCol = if x + ballRadius >= wallRight
                      then Just (-1,0)
                      else Nothing
