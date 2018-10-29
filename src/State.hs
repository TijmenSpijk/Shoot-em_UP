module State where

import Entities

data Game = Game {
    gameState :: GameState,
    player :: Player,
    bullets :: [Bullet],
    enemies :: [Enemy]
}

data GameState = Pause
               | Play

initialState :: Game
initialState = Game {
    gameState = Play,
    player = Player {
        playerState = Alive, 
        playerHealth = baseHealth, 
        playerPosition = startingPoint, 
        playerMovement = baseMovement,
        powerUp = basePowerUp},
    enemies = [enemy1, enemy2],
    bullets = []
}
    where enemy1 = Enemy {enemyState = setEnemyState,
                          enemyHealth = setEnemyHealth,
                          enemyPosition = setEnemyPosition,
                          enemyMovement = setEnemyMovement,
                          enemyType = setEnemyType}
          enemy2 = Enemy {enemyState = setEnemyState,
                          enemyHealth = setEnemyHealth,
                          enemyPosition = (700, 100),
                          enemyMovement = setEnemyMovement,
                          enemyType = setEnemyType}

-- Creation of Entities
makeBullets :: Game -> Game
makeBullets game = game {bullets = newBullet : bulletsList}
    where newBullet = Bullet {bulletPosition = getPlayerPosition (player game),
                              bulletMovement = baseBulletMovement}
          bulletsList = bullets game