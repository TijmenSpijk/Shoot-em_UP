module Entities where

data Player = Player {
    playerState :: EntityState,
    playerHealth :: Health,
    playerPosition :: (Float, Float),
    playerMovement :: (Float, Float),
    powerUp :: PowerUp
}

data Enemy = Enemy {
    enemyState :: EntityState,
    enemyHealth :: Health,
    enemyPosition :: (Float, Float),
    enemyMovement :: (Float, Float),
    enemyType :: EnemyType
}

data Bullet = Bullet {
    bulletPosition :: (Float, Float),
    bulletMovement :: (Float, Float)
}

data EnemyType = Soldier Health Energy
               | Heavy Health Energy
               | Runner Health Energy

data Health = Health Int

data EntityState = Alive
                  | Dead

data PowerUp = ExtraHealth Energy
             | RapidFire Energy
             | Nuke Energy
             | Empty Energy

data Energy = Energy Int

-- Base values (mainly for player)
baseHealth :: Health
baseHealth = Health 100

baseEnergy :: Energy
baseEnergy = Energy 0

startingPoint :: (Float, Float)
startingPoint = (-600, 0)

baseMovement :: (Float, Float)
baseMovement = (0, 0)

basePowerUp :: PowerUp
basePowerUp = Empty (Energy 0)

baseBulletMovement :: (Float, Float)
baseBulletMovement = (150, 0)

-- setup Enemy
setEnemyState :: EntityState
setEnemyState = Alive

setEnemyHealth :: Health
setEnemyHealth = Health 100

setEnemyPosition :: (Float, Float)
setEnemyPosition = (700, 0)

setEnemyMovement :: (Float, Float)
setEnemyMovement = (-100, 0)

setEnemyType :: EnemyType
setEnemyType = Soldier (Health 100) (Energy 10)


-- get functies voor Player
getPlayerState :: Player -> EntityState
getPlayerState player = playerState player

getPlayerHealth :: Player -> Health
getPlayerHealth player = playerHealth player

getPlayerPosition :: Player -> (Float, Float)
getPlayerPosition player = playerPosition player

getPlayerMovement :: Player -> (Float, Float)
getPlayerMovement player = playerMovement player

getPlayerPowerUp :: Player -> PowerUp
getPlayerPowerUp player = powerUp player

-- get functies voor Enemies
getEnemyState :: Enemy -> EntityState
getEnemyState enemy = enemyState enemy

getEnemyHealth :: Enemy -> Health
getEnemyHealth enemy = enemyHealth enemy

getEnemyPosition :: Enemy -> (Float, Float)
getEnemyPosition enemy = enemyPosition enemy

getEnemyMovement :: Enemy -> (Float, Float)
getEnemyMovement enemy = enemyMovement enemy

getEnemyType :: Enemy -> EnemyType
getEnemyType enemy = enemyType enemy

-- get functies voor Bullets
getBulletPosition :: Bullet -> (Float, Float)
getBulletPosition bullet = bulletPosition bullet

getBulletMovement :: Bullet -> (Float, Float)
getBulletMovement bullet = bulletMovement bullet


