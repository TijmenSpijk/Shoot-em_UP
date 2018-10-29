module Controller where

import State
import Entities
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort


movePlayer :: Float -> Game -> Game
movePlayer seconds game = 
    game {player = 
        Player {playerState = getPlayerState (player game),
                playerHealth = getPlayerHealth (player game),
                playerPosition = (x', y'),
                playerMovement = case p of
                    True -> (vx, vy)
                    False -> (0,0),
                powerUp = getPlayerPowerUp (player game)}}
    where (x, y) = playerPosition (player game)
          (vx, vy) = playerMovement (player game)
          x' = x + vx * seconds
          y' = case p of
            True -> y + vy * seconds
            False -> y - (0.05 * vy)
          p = snd(getPlayerPosition (player game)) <= 345 && snd(getPlayerPosition (player game)) >= -345

moveEnemies :: Float -> Game -> Game
moveEnemies seconds game = 
    game {enemies = Enemy {enemyState = Alive,
                            enemyHealth = Health 100,
                            enemyPosition = (x', y'),
                            enemyMovement = case p of
                                True -> (vx, vy)
                                False -> (vx, -vy),
                            enemyType = Soldier Health 100 Energy 10 }}
    where (x, y) = enemyPosition ()


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
handleKeys (EventKey (SpecialKey KeySpace) _ _ _) game = 
    game {player = 
        Player {playerState = getPlayerState (player game),
                playerHealth = getPlayerHealth (player game),
                playerPosition = getPlayerPosition (player game),
                playerMovement = (0, 0),
                powerUp = getPlayerPowerUp (player game)}}
handleKeys _ game = game