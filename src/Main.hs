module Main where

import Constants
import Rendering
import Physics
import Reflex.Gloss
import Reflex.Class
import Reflex.Dynamic
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Graphics.Gloss.Data.Display
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Vector


main :: IO ()
main = do
     playReflex (InWindow "Pong" (1920,1080) (0,0)) black 60 glossPong


pongInput :: InputEvent -> Point -> Point
pongInput (EventMotion position) _ = position
pongInput _ state = state


glossPong refreshEvents inputEvents = 
  do userInputs <- foldDyn pongInput (0,0) inputEvents
                           
     -- Every refresh query the user's inputs.
     let comb = attachDyn userInputs refreshEvents
                          
     -- Run simulation and render.
     sim <- foldDyn (uncurry pongSimulate) start comb
     return . fmap renderPong $ current sim

  where start = PongState (Ball (0,0) (600,600)) player enemy
        player = Box (playerX, 0) paddleWidth paddleHeight
        enemy = Box (enemyX, 0) paddleWidth paddleHeight
