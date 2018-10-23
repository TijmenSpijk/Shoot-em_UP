module Controller where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort
import State

movePlayer :: Float -> Game -> Game
movePlayer seconds game = game {playerLocation = (x', y')}
    where (x, y) = playerLocation game
          (vx, vy) = playerMovement game
          x' = x + vx * seconds
          y' = y + vy * seconds

handleKeys :: Event -> Game -> Game
handleKeys (EventKey (Char 'w') _ _ _) game = game { playerMovement = (0,30)}
handleKeys (EventKey (Char 's') _ _ _) game = game { playerMovement = (0,-30)}
handleKeys (EventKey (SpecialKey KeySpace) _ _ _) game = game { playerMovement = (0,0)}
handleKeys _ game = game