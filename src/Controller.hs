module Controller where

import State
import Entities
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort

moveEntities :: Float -> Game -> Game
moveEntities seconds game = case gameState game of
    Play -> game {player = movePlayer seconds (player game),
                  enemies = map (moveEnemy seconds) (enemies game),
                  bullets = map (moveBullet seconds) (bullets game)}
    Pause -> game    
                                
movePlayer :: Float -> Player -> Player
movePlayer seconds player = 
        Player {playerState = getPlayerState player,
                playerHealth = getPlayerHealth player,
                playerPosition = (x', y'),
                playerMovement = case p of
                    True -> (vx, vy)
                    False -> (0,0),
                powerUp = getPlayerPowerUp player}
    where (x, y) = playerPosition player
          (vx, vy) = playerMovement player
          x' = x 
          y' = case p of
            True -> y + vy * seconds
            False -> y - (0.05 * vy)
          p = snd(getPlayerPosition player) <= 345 && snd(getPlayerPosition player) >= -345

moveEnemy :: Float -> Enemy -> Enemy
moveEnemy seconds enemy = 
        Enemy {enemyState = getEnemyState enemy,
               enemyHealth = getEnemyHealth enemy,
               enemyPosition = (x', y'),
               enemyMovement = (vx, vy),
               enemyType = getEnemyType enemy}
    where (x, y) = getEnemyPosition enemy
          (vx, vy) = getEnemyMovement enemy
          x' = case fst(getEnemyPosition enemy) < (-640) of
            True -> x
            False -> x + vx * seconds                                                
          y' = y                                                       

moveBullet :: Float -> Bullet -> Bullet
moveBullet seconds bullet = 
        Bullet {bulletPosition = (x', y'),
                bulletMovement = (vx, vy)}
    where (x, y) = getBulletPosition bullet
          (vx, vy) = getBulletMovement bullet
          x' = x + vx * seconds
          y' = y 

handleAllKeys :: Event -> Game -> Game
handleAllKeys (EventKey (Char 'p') _ _ _) game = case gameState game of 
    Play -> game {gameState = Pause}
    Pause -> game {gameState = Play}
handleAllKeys event game = case gameState game of 
    Play -> handleKeys event game
    Pause -> game

handleKeys :: Event -> Game -> Game
handleKeys (EventKey (Char 'w') _ _ _) game = 
    game {player = 
        Player {playerState = getPlayerState (player game),
                playerHealth = getPlayerHealth (player game),
                playerPosition = getPlayerPosition (player game),
                playerMovement = (0, 100),
                powerUp = getPlayerPowerUp (player game)}}
handleKeys (EventKey (Char 's') _ _ _) game = 
    game {player = 
        Player {playerState = getPlayerState (player game),
                playerHealth = getPlayerHealth (player game),
                playerPosition = getPlayerPosition (player game),
                playerMovement = (0, -100),
                powerUp = getPlayerPowerUp (player game)}}
handleKeys (EventKey (SpecialKey KeySpace) _ _ _) game = makeBullets game
handleKeys _ game = game