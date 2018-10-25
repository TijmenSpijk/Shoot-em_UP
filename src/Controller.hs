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
                playerMovement = (vx, vy),
                powerUp = getPlayerPowerUp (player game)}}
    where (x, y) = playerPosition (player game)
          (vx, vy) = playerMovement (player game)
          x' = x + vx * seconds
          y' = y + vy * seconds

handleKeys :: Event -> Game -> Game
handleKeys (EventKey (Char 'w') _ _ _) game = 
    game {player = 
        Player {playerState = getPlayerState (player game),
                playerHealth = getPlayerHealth (player game),
                playerPosition = getPlayerPosition (player game),
                playerMovement = case (snd(getPlayerPosition (player game))) < 700 of
                    True -> (0, 100)
                    False -> (0,0),
                powerUp = getPlayerPowerUp (player game)}}
handleKeys (EventKey (Char 's') _ _ _) game = 
    game {player = 
        Player {playerState = getPlayerState (player game),
                playerHealth = getPlayerHealth (player game),
                playerPosition = getPlayerPosition (player game),
                playerMovement = case (snd(getPlayerPosition (player game))) > 0 of
                    True -> (0, -100)
                    False -> (0,0),
                powerUp = getPlayerPowerUp (player game)}}
handleKeys (EventKey (SpecialKey KeySpace) _ _ _) game = 
    game {player = 
        Player {playerState = getPlayerState (player game),
                playerHealth = getPlayerHealth (player game),
                playerPosition = getPlayerPosition (player game),
                playerMovement = (0, 0),
                powerUp = getPlayerPowerUp (player game)}}
handleKeys _ game = game