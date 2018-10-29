module View where

import State 
import Entities
import Graphics.Gloss

render :: Game -> Picture
render game = pictures (players : enemiess)
    where players = renderPlayer (player game)
          enemiess = map renderEnemy (enemies game)

renderPlayer :: Player -> Picture
renderPlayer player = uncurry translate (playerLocation) $ color playerColor $ circleSolid 10
    where playerColor = dark red
          playerLocation = getPlayerPosition player

renderEnemy :: Enemy -> Picture
renderEnemy enemy = uncurry translate (enemyLocation) $ color enemyColor $ circleSolid 12
    
    where enemyColor = light blue
          enemyLocation = getEnemyPosition enemy

fps :: Int
fps = 60

window :: Display
window = InWindow "Shoot'em Up" (1280, 720) (0, 0)

background :: Color
background = black