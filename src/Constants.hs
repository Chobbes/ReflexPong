module Constants where

import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.Pure.Game


data PongState = PongState { ballState :: Ball
                           , playerY :: Float
                           , enemyY :: Float
                           }


data Ball = Ball { ballPos :: Point 
                 , ballVel :: Vector
                 }


ballRadius :: Float
ballRadius = 5

playerX :: Float
playerX = -700

enemyX :: Float
enemyX = 700

ballColour :: Color
ballColour = white

playerColour :: Color
playerColour = white

enemyColour :: Color
enemyColour = white

wallColour :: Color
wallColour = white

paddleWidth :: Float
paddleWidth = 10

paddleHeight :: Float
paddleHeight = 50


wallTop :: Float
wallTop = 500

wallBottom :: Float
wallBottom = -500

wallLeft :: Float
wallLeft = -710

wallRight :: Float
wallRight = 710
