{-# LANGUAGE TupleSections #-}

module Physics where

import Constants
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Pure.Game
import Control.Applicative
import Debug.Trace
import Data.Maybe


pongSimulate :: Point -> Float -> PongState -> PongState
pongSimulate (_,my) step state = runPhysics playerState step
  where playerState = state { playerPaddle = newPlayer }
        newPlayer = updateY (playerPaddle state) my

updateY :: Box -> Float -> Box
updateY b@(Box (x,_) _ _) newY = b {boxCentre = (x, newY)}


reflect :: Vector -> Vector -> Vector
reflect axis v = v `subV` ((2 * (dotV axis v)) `mulSV` axis)
  where subV (x1,y1) (x2,y2) = (x1 - x2, y1 - y2)


-- | Run physics on the ball, returning the new state after a timestep.
runPhysics :: PongState -> Float -> PongState
runPhysics state step =
    case collision of
       Nothing -> let newBall = moveBall ball step
                      ballY = snd (ballPos newBall)
                      enemy = enemyPaddle state
                      newEnemy = updateY enemy ballY
                  in state { ballState = newBall, enemyPaddle = newEnemy }
       Just (dist, norm) -> let advancedBall = moveBall ball dist
                                newBall = bounceBall ball norm
                                newState = state { ballState = moveBall newBall 0.0001 }
                            in runPhysics newState (step - dist)
  where collision = ballCollisions state step
        ball = ballState state


moveBall :: Ball -> Float -> Ball
moveBall (Ball (x,y) (vx,vy)) t = Ball (x + t * vx, y + t * vy) (vx,vy)


bounceBall :: Ball -> Vector -> Ball
bounceBall ball@(Ball _ v) norm = ball { ballVel = reflect norm v }


ballCollisions :: PongState -> Float -> Maybe (Float, Vector)
ballCollisions (PongState ball player enemy) step = 
  case collision of
    Nothing -> Nothing
    col@(Just (dist,_)) -> if dist > step
                              then Nothing
                              else collision
                                   
  where collision = minMaybe $ catMaybes [playerCollision, enemyCollision, wallCollision]
         where wallCollision = boxBallCollision wallBox ball
               playerCollision = paddleBallCollision player (1,1) (1,-1) ball
               enemyCollision = paddleBallCollision enemy (-1,1) (-1,-1) ball

-- Need to simulate collisions with geometry accurately.  -- Use a ray
-- cast to find the intersection point, bounce, ray cast again to find
-- new location until time is up.

-- | (time step, normal)
boxBallCollision :: Box -> Ball -> Maybe (Float, Vector)
boxBallCollision box (Ball pos vel) = 
  minMaybe $ catMaybes [top, bottom, left, right]

  where top = (,(0,1)) <$> rayHorizontal ori vel (leftX, rightX) topY
        bottom = (,(0,-1)) <$> rayHorizontal ori vel (leftX, rightX) bottomY

        left = (,(-1,0)) <$> rayVertical ori vel (bottomY, topY) leftX
        right = (,(1,0)) <$> rayVertical ori vel (bottomY, topY) rightX

        ori = pos `addV` (ballRadius `mulSV` (normalizeV vel))

        (topY, bottomY, leftX, rightX) = boxCoords box


paddleBallCollision :: Box -> Vector -> Vector -> Ball -> Maybe (Float, Vector)
paddleBallCollision box topV bottomV ball = (,norm) . fst <$> collision
  where collision = boxBallCollision box ball
        Just (dist, _) = collision
        norm = normalizeV (tx + bx, ty + by)

        relY = (ballY - boxY + h) / paddleHeight
        (tx,ty) = relY `mulSV` topV
        (bx,by) = (1 - relY) `mulSV` bottomV
        
        ballY = snd . ballPos $ moveBall ball dist
        boxY = snd . boxCentre $ box
        h = paddleHeight / 2


addV :: Vector -> Vector -> Vector
addV (x1,y1) (x2,y2) = (x1+x2, y1+y2)


minMaybe :: Ord a => [a] -> Maybe a
minMaybe [] = Nothing
minMaybe xs = Just $ minimum xs


boxCoords :: Box -> (Float, Float, Float, Float)
boxCoords (Box (x,y) w h) = (y + h', y - h', x - w', x + w')
  where h' = h / 2
        w' = w / 2


rayHorizontal :: Point -> Vector -> (Float, Float) -> Float -> Maybe Float
rayHorizontal (oriX,oriY) (dirX,dirY) xRange y = 
  if dist >= 0 && dirY /= 0 && inRange xRange x
     then Just dist 
     else Nothing
  where dist = (y - oriY) / dirY
        x = dist * dirX + oriX


rayVertical :: Point -> Vector -> (Float, Float) -> Float -> Maybe Float
rayVertical (oriX,oriY) (dirX,dirY) yRange x = 
  if dist >= 0 && dirX /= 0 && inRange yRange y
     then Just dist 
     else Nothing
  where dist = (x - oriX) / dirX
        y = dist * dirY + oriY


inRange :: (Show a, Ord a) => (a,a) -> a -> Bool
inRange (l,u) x = l <= x && x <= u
