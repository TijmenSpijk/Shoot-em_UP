module View where

import State 
import Graphics.Gloss

render :: Game -> Picture
render game = pictures [player]
    where player = uncurry translate (playerLocation game) $ color playerColor $ circleSolid 10
          playerColor = dark red

fps :: Int
fps = 60

window :: Display
window = InWindow "Shoot'em Up" (1280, 720) (10, 10)

background :: Color
background = black

drawing :: Picture
drawing = render initialState