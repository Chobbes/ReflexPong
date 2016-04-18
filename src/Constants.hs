module Constants where

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Pure.Game


data PongState = PongState { ballState :: Ball
                           , playerPaddle :: Box
                           , enemyPaddle :: Box
                           } deriving (Show)


data Ball = Ball { ballPos :: Point 
                 , ballVel :: Vector
                 } deriving (Show)


data Box = Box { boxCentre :: Point
               , boxWidth :: Float
               , boxHeight :: Float
               } deriving (Show)


-- | Size of the ball.
ballRadius :: Float
ballRadius = 5


-- | Location of the player.
playerX :: Float
playerX = -700


-- | Location of the enemy.
enemyX :: Float
enemyX = 700


-- | Colour of the ball.
ballColour :: Color
ballColour = white


-- | Colour of the player's paddle.
playerColour :: Color
playerColour = white


-- | Colour of the enemy's paddle.
enemyColour :: Color
enemyColour = white


-- | Colour of the walls.
wallColour :: Color
wallColour = white


-- | Width of paddles.
paddleWidth :: Float
paddleWidth = 10


-- | Height of paddles.
paddleHeight :: Float
paddleHeight = 50


-- Wall Dimensions
wallTop :: Float
wallTop = 500


wallBottom :: Float
wallBottom = -500


wallLeft :: Float
wallLeft = playerX - paddleWidth


wallRight :: Float
wallRight = enemyX + paddleWidth


-- | Box for the walls.
wallBox :: Box
wallBox = Box { boxCentre = (centreX,centreY)
              , boxWidth = wallRight - wallLeft
              , boxHeight = wallTop - wallBottom
              }
  where centreX = (wallTop + wallBottom) / 2
        centreY = (wallLeft + wallRight) / 2
