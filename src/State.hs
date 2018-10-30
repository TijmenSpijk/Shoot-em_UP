module State where

import Entities

data Game = Game {
    gameState :: GameState,
    player :: Player,
    bullets :: [Bullet],
    enemies :: [Enemy],
    buttonW :: ButtonState,
    buttonS :: ButtonState,
    buttonP :: ButtonState,
    spawnTime :: Float
}

data GameState = Pause
               | Play

data ButtonState = On
                 | Off

initialState :: Game
initialState = Game {
    gameState = Play,
    player = Player {
        playerState = Alive, 
        playerHealth = baseHealth, 
        playerPosition = startingPoint, 
        playerMovement = baseMovement,
        powerUp = basePowerUp},
    enemies = [],
    bullets = [],
    buttonW = Off, buttonS = Off, buttonP = Off,
    spawnTime = 0
}
    where enemy1 = Enemy {enemyState = setEnemyState,
                          enemyHealth = setEnemyHealth,
                          enemyPosition = setEnemyPosition 20,
                          enemyMovement = setEnemyMovement,
                          enemyType = setEnemyType}
          enemy2 = Enemy {enemyState = setEnemyState,
                          enemyHealth = setEnemyHealth,
                          enemyPosition = setEnemyPosition (-20),
                          enemyMovement = setEnemyMovement,
                          enemyType = setEnemyType}

-- Creation of Entities
makeBullets :: Game -> Game
makeBullets game = game {bullets = newBullet : bulletsList}
    where newBullet = Bullet {bulletPosition = getPlayerPosition (player game),
                              bulletMovement = baseBulletMovement}
          bulletsList = bullets game

makeEnemies :: Float -> Float -> Game -> Game
makeEnemies seconds randomnumber game   | spawnTime game > 1 = game {enemies = newEnemy:enemyList,
                                                                      spawnTime = 0}
                                        | otherwise = game {spawnTime = spawnTime game + seconds}
    where
        newEnemy = Enemy {  enemyState = setEnemyState,
                            enemyHealth = setEnemyHealth,
                            enemyPosition = setEnemyPosition randomnumber,
                            enemyMovement = setEnemyMovement,
                            enemyType = setEnemyType}
        enemyList = enemies game