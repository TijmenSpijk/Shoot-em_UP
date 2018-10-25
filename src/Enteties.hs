module Enteties where

data Player = Player {
    playerState :: EntetieState,
    playerHealth :: Health,
    playerPosition :: (Float, Float),
    playerMovement :: (Float, Float),
    powerUp :: PowerUp
}

data Enemy = Enemy {
    enemyState :: EntetieState,
    enemyHealth :: Health,
    enemyPosition :: (Float, Float),
    enemyType :: EnemyType
}

data EnemyType = Soldier Health Energy
               | Heavy Health Energy
               | Runner Health Energy

data Health = Health Int

data EntetieState = Alive
                  | Dead

data PowerUp = ExtraHealth Energy
             | RapidFire Energy
             | Nuke Energy
             | Empty Energy

data Energy = Energy Int

baseHealth :: Health
baseHealth = Health 100

baseEnergy :: Energy
baseEnergy = Energy 0

startingPoint :: (Float, Float)
startingPoint = (-200, 0)

baseMovement :: (Float, Float)
baseMovement = (0, 0)

basePowerUp :: PowerUp
basePowerUp = Empty (Energy 0)

-- get functies voor Player
getPlayerState :: Player -> EntetieState
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
getEnemyState :: Enemy -> EntetieState
getEnemyState enemy = enemyState enemy

getEnemyHealth :: Enemy -> Health
getEnemyHealth enemy = enemyHealth enemy

getEnemyPosition :: Enemy -> (Float, Float)
getEnemyPosition enemy = enemyPosition enemy

getEnemyType :: Enemy -> EnemyType
getEnemyType enemy = enemyType enemy