module State where

import Entities

data Game = Game {
    gameState :: GameState,
    score :: Int,
    energy :: Int,
    player :: Player,
    bullets :: [Bullet],
    enemies :: [Enemy],
    spawnTime :: Float,
    spawnRate :: Float
}

data GameState = Pause
               | Play

initialState :: Game
initialState = Game {
    gameState = Play,
    score = 1234, energy = 1234,
    player = Player {
        playerState = Alive, 
        playerHealth = baseHealth, 
        playerPosition = startingPoint, 
        playerMovement = baseMovement,
        powerUp = basePowerUp},
    enemies = [], bullets = [],
    spawnTime = 0, spawnRate = 1
}

-- Creation of Entities
makeBullets :: Game -> Game
makeBullets game = game {bullets = newBullet : bulletsList}
    where newBullet = Bullet {bulletPosition = getPlayerPosition (player game),
                              bulletMovement = baseBulletMovement,
                              bulletCollide  = False}
          bulletsList = bullets game

makeEnemies :: Float -> Float -> Game -> Game
makeEnemies seconds randomnumber game   | spawnTime game > spawnRate game = case gameState game of
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