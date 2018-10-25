module State where

import Enteties

data Game = Game {
    player :: Player
} 

initialState :: Game
initialState = Game {
    player = Player {
        playerState = Alive, 
        playerHealth = baseHealth, 
        playerPosition = startingPoint, 
        playerMovement = baseMovement,
        powerUp = basePowerUp}   
}
