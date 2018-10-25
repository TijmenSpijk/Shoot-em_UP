module View where

import State 
import Enteties
import Graphics.Gloss

render :: Game -> Picture
render game = pictures [players]
    where players = uncurry translate (playerLocation) $ color playerColor $ circleSolid 10
          playerColor = dark red
          playerLocation = getPlayerPosition (player game)

fps :: Int
fps = 60

window :: Display
window = InWindow "Shoot'em Up" (1280, 720) (10, 10)

background :: Color
background = black
