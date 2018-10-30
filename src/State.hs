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
    buttonSpace :: ButtonState,
    spawnTime :: Float,
    spawnRate :: Float
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
    enemies = [], bullets = [],
    buttonW = Off, buttonS = Off, buttonP = Off, buttonSpace = Off,
    spawnTime = 0, spawnRate = 3
}

-- Creation of Entities
makeBullets :: Game -> Game
makeBullets game = game {bullets = newBullet : bulletsList}
    where newBullet = Bullet {bulletPosition = getPlayerPosition (player game),
                              bulletMovement = baseBulletMovement}
          bulletsList = bullets game

makeEnemies :: Float -> Float -> Game -> Game
makeEnemies seconds randomnumber game   | spawnTime game > 1 = case gameState game of
                                            Play -> game {enemies = newEnemy:enemyList, spawnTime = 0}
                                            Pause -> game 
                                        | otherwise = case gameState game of
                                            Play -> game {spawnTime = spawnTime game + seconds}
                                            Pause -> game
    where
        newEnemy = Enemy {  enemyState = setEnemyState,
                            enemyHealth = setEnemyHealth,
                            enemyPosition = setEnemyPosition randomnumber,
                            enemyMovement = setEnemyMovement,
                            enemyType = setEnemyType}
        enemyList = enemies game