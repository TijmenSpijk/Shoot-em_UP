module State where

import Entities

data Game = Game {
    player :: Player,
    enemies :: Enemy
} 

initialState :: Game
initialState = Game {
    player = Player {
        playerState = Alive, 
        playerHealth = baseHealth, 
        playerPosition = startingPoint, 
        playerMovement = baseMovement,
        powerUp = basePowerUp},
    enemies = [enemy1]   
}
    where enemy1 = Enemy {
            enemyState = setEnemyState,
            enemyHealth = setEnemyHealth,
            enemyPosition = setEnemyPosition,
            enemyMovement = setEnemyMovement,
            enemyType = setEnemyType
    }