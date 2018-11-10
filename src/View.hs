module View where

import State 
import Entities
import Graphics.Gloss

render :: Game -> IO Picture
render game = return (renderPure game)

renderPure :: Game -> Picture
renderPure game = pictures (menu' : score' : health' : player' : (enemies' ++ bullets'))
    where player' = renderPlayer (player game)
          score' = (renderScore (score game))
          health' = (renderHealth (getPlayerHealth (player game)))
          enemies' = map renderEnemy (enemies game)
          bullets' = map renderBullet (bullets game)
          menu' = renderMenu

renderPlayer :: Player -> Picture
renderPlayer player = uncurry translate (playerLocation) $ color playerColor $ circleSolid 10
    where playerColor = dark red
          playerLocation = getPlayerPosition player

renderEnemy :: Enemy -> Picture
renderEnemy enemy = uncurry translate (enemyLocation) $ color enemyColor $ circleSolid 12    
    where enemyColor = light blue
          enemyLocation = getEnemyPosition enemy

renderBullet :: Bullet -> Picture
renderBullet bullet = uncurry translate (bulletLocation) $ color bulletColor $ rectangleSolid 8 2
    where bulletColor = light green
          bulletLocation = getBulletPosition bullet

renderMenu :: Picture
renderMenu = uncurry translate (menuLocation) $ color menuColor $ rectangleSolid 1280 50
    where menuColor = white
          menuLocation = (0, 340)

renderScore :: Int -> Picture
renderScore score = uncurry translate (scoreLocation) $ scale 0.2 0.2 $ color scoreColor $ text ("Score: " ++ show score)
    where scoreColor = black
          scoreLocation = (-620, 330)

renderHealth :: Health -> Picture
renderHealth health = uncurry translate (healthLocation) $ scale 0.2 0.2 $ color healthColor $ text (show health)
    where healthColor = black
          healthLocation = (475, 330)

fps :: Int
fps = 60

window :: Display
window = InWindow "Shoot'em Up" (1280, 720) (0, 0)

background :: Color
background = black