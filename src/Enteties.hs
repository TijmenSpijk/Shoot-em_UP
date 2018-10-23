module Enteties where

data Player = Player {
    playerState :: EntetieState,
    playerHealth :: Health,
    playerPosition :: Point,
    powerUp :: PowerUp
}

data Enemy = Enemy {
    enemyState :: EntetieState,
    enemyHealth :: Health,
    enemyPosition :: Point,
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

data Energy = Energy Float

baseHealth :: Health
baseHealth = Health 100

baseEnergy :: Energy
baseEnergy = Energy 0

startingPoint :: Point
startingPoint = Point 0 -100

basePowerUp :: PowerUp
basePowerUp = Empty 0

getPlayerLocation :: Player -> Point
getPlayerLocation player = playerPosition player

getEnemyLocation :: Enemy -> Point
getEnemyLocation enemy = enemyPosition enemy