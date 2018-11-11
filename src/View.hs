module View where

import State 
import Entities
import Graphics.Gloss

render :: Game -> IO Picture
render game = return (renderPure game)

renderPure :: Game -> Picture
renderPure game = pictures (end : menubar' : score' : health' : info : player' : (enemies' ++ bullets'))
    where player' = renderPlayer (player game)
          score' = (renderScore (score game))
          health' = (renderHealth (getPlayerHealth (player game)))
          enemies' = map renderEnemy (enemies game)
          bullets' = map renderBullet (bullets game)
          menubar' = renderMenuBar
          end = renderEnd (player game)
          info = renderInfo

renderPlayer :: Player -> Picture
renderPlayer player = uncurry translate (playerLocation) $ color (dark red) $ circleSolid 10
    where playerLocation = getPlayerPosition player

renderEnemy :: Enemy -> Picture
renderEnemy enemy = uncurry translate (enemyLocation) $ color (light blue) $ circleSolid 12    
    where enemyLocation = getEnemyPosition enemy

renderBullet :: Bullet -> Picture
renderBullet bullet = uncurry translate (bulletLocation) $ color (light green) $ rectangleSolid 8 2
    where bulletLocation = getBulletPosition bullet

renderMenuBar :: Picture
renderMenuBar = uncurry translate (0, 340) $ color white $ rectangleSolid 1280 50

renderScore :: Int -> Picture
renderScore score = uncurry translate (-620, 330) $ scale 0.2 0.2 $ color black $ text ("Score: " ++ show score)

renderHealth :: Health -> Picture
renderHealth health = uncurry translate (475, 330) $ scale 0.2 0.2 $ color black $ text (show health)

renderInfo :: Picture
renderInfo = uncurry translate (-250, 330) $ scale 0.2 0.2 $ color black $ text ("p = Pause     r = Restart")

renderEnd :: Player -> Picture
renderEnd player = case getPlayerState player of
    Alive -> uncurry translate (0, 370) $ color white $ rectangleSolid 1 1
    Dead  -> uncurry translate (-250, 0) $ scale 0.5 0.5 $ color red $ text ("You are Dead")

fps :: Int
fps = 60

window :: Display
window = InWindow "Shoot'em Up" (1280, 720) (0, 0)

background :: Color
background = black