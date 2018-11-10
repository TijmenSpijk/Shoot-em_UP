module Controller where

import State
import Entities
import System.Random
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort

step :: Float -> Game -> IO Game
step seconds game =  do
    randomnumber <- getStdRandom (randomR (-350,300))
    return $ checkSpawn $ checkDead $ detectCollisions $ moveEntities seconds $ makeEnemies seconds randomnumber game

moveEntities :: Float -> Game -> Game
moveEntities seconds game = case gameState game of
    Play -> game {player = movePlayer seconds (player game),
                  enemies = map (moveEnemy seconds) (enemies game),
                  bullets = map (moveBullet seconds) (bullets game)}
    Pause -> game    
                                
movePlayer :: Float -> Player -> Player
movePlayer seconds player = 
        Player {playerState = getPlayerState player,
                playerHealth = getPlayerHealth player,
                playerPosition = (x', y'),
                playerMovement = case p of
                    True -> (vx, vy)
                    False -> (0,0),
                powerUp = getPlayerPowerUp player}
    where (x, y) = playerPosition player
          (vx, vy) = playerMovement player
          x' = x 
          y' = case p of
            True -> y + vy * seconds
            False -> y - (0.05 * vy)
          p = snd(getPlayerPosition player) <= 300 && snd(getPlayerPosition player) >= -345

moveEnemy :: Float -> Enemy -> Enemy
moveEnemy seconds enemy = 
        Enemy {enemyState = getEnemyState enemy,
               enemyHealth = getEnemyHealth enemy,
               enemyPosition = (x', y'),
               enemyMovement = (vx, vy),
               enemyType = getEnemyType enemy}
    where (x, y) = getEnemyPosition enemy
          (vx, vy) = getEnemyMovement enemy
          x' = case fst(getEnemyPosition enemy) < (-640) of
            True -> x
            False -> x + vx * seconds                                                
          y' = y                                                       

moveBullet :: Float -> Bullet -> Bullet
moveBullet seconds bullet = 
        Bullet {bulletPosition = (x', y'),
                bulletMovement = (vx, vy),
                bulletCollide = getBulletCollide bullet}
    where (x, y) = getBulletPosition bullet
          (vx, vy) = getBulletMovement bullet
          x' = x + vx * seconds
          y' = y 


input :: Event -> Game -> IO Game
input event game = return (handleAllKeys event game)

handleAllKeys :: Event -> Game -> Game
handleAllKeys (EventKey (Char 'p') Down _ _) game = case gameState game of 
    Play -> game {gameState = Pause}
    Pause -> game {gameState = Play}
handleAllKeys event game = case gameState game of 
    Play -> handleKeys event game
    Pause -> game

handleKeys :: Event -> Game -> Game
handleKeys (EventKey (Char 'w') Down _ _) game =
    game {player = 
            Player {playerState = getPlayerState (player game),
                    playerHealth = getPlayerHealth (player game),
                    playerPosition = getPlayerPosition (player game),
                    playerMovement = (0, 100),
                    powerUp = getPlayerPowerUp (player game)}}
handleKeys (EventKey (Char 'w') Up _ _) game = 
    game {player = 
            Player {playerState = getPlayerState (player game),
                    playerHealth = getPlayerHealth (player game),
                    playerPosition = getPlayerPosition (player game),
                    playerMovement = (0, 0),
                    powerUp = getPlayerPowerUp (player game)}}
handleKeys (EventKey (Char 's') Down _ _) game =
    game {player = 
            Player {playerState = getPlayerState (player game),
                    playerHealth = getPlayerHealth (player game),
                    playerPosition = getPlayerPosition (player game),
                    playerMovement = (0, -100),
                    powerUp = getPlayerPowerUp (player game)}}
handleKeys (EventKey (Char 's') Up _ _) game =
    game {player = 
            Player {playerState = getPlayerState (player game),
                    playerHealth = getPlayerHealth (player game),
                    playerPosition = getPlayerPosition (player game),
                    playerMovement = (0, 0),
                    powerUp = getPlayerPowerUp (player game)}}
handleKeys (EventKey (SpecialKey KeySpace) Down _ _) game = makeBullets game  
handleKeys _ game = game

-- Collisions
detectCollisions :: Game -> Game
detectCollisions game = game {bullets = bulletList, enemies = enemyList}
    where enemyList  = detectEnemyCollisions  (bullets game) (enemies game)
          bulletList = detectBulletCollisions (bullets game) (enemies game)

detectBulletCollisions :: [Bullet] -> [Enemy] -> [Bullet]
detectBulletCollisions [] _ = []
detectBulletCollisions bulletList [] = bulletList
detectBulletCollisions [bullet] enemyList = [detectBulletCollision bullet enemyList]
detectBulletCollisions (bullet:bulletRest) enemyList = (detectBulletCollision bullet enemyList) : (detectBulletCollisions bulletRest enemyList)

detectBulletCollision :: Bullet -> [Enemy] -> Bullet
detectBulletCollision bullet [] = bullet
detectBulletCollision bullet [x] | detectRealCollision bullet x = Bullet       {bulletPosition = getBulletPosition bullet,
                                                                                bulletMovement = baseBulletMovement,
                                                                                bulletCollide  = True}
                                 | otherwise = bullet 
detectBulletCollision bullet (x:rest) | detectRealCollision bullet x = Bullet  {bulletPosition = getBulletPosition bullet,
                                                                                bulletMovement = baseBulletMovement,
                                                                                bulletCollide  = True}
                                      | otherwise = detectBulletCollision bullet rest

detectEnemyCollisions :: [Bullet] -> [Enemy] -> [Enemy]
detectEnemyCollisions _ [] = []
detectEnemyCollisions [] enemyList = enemyList
detectEnemyCollisions bulletList [enemy] = [detectEnemyCollision bulletList enemy]
detectEnemyCollisions bulletList (enemy:enemyRest) = (detectEnemyCollision bulletList enemy) : (detectEnemyCollisions bulletList enemyRest)

detectEnemyCollision :: [Bullet] -> Enemy -> Enemy
detectEnemyCollision [] enemy = enemy
detectEnemyCollision [x] enemy | detectRealCollision x enemy = Enemy {enemyState = Dead,
                                                                      enemyHealth = getEnemyHealth enemy,
                                                                      enemyPosition = getEnemyPosition enemy,
                                                                      enemyMovement = getEnemyMovement enemy,
                                                                      enemyType = getEnemyType enemy}
                               | otherwise = enemy
detectEnemyCollision (x:rest) enemy | detectRealCollision x enemy = Enemy {enemyState = Dead,
                                                                           enemyHealth = getEnemyHealth enemy,
                                                                           enemyPosition = getEnemyPosition enemy,
                                                                           enemyMovement = getEnemyMovement enemy,
                                                                           enemyType = getEnemyType enemy}
                                    | otherwise = detectEnemyCollision rest enemy                                       

detectRealCollision :: Bullet -> Enemy -> Bool 
detectRealCollision bullet enemy | (bulletYplus <= enemyYmin) && (bulletYplus >= enemyYplus) && (bulletXplus >= enemyXmin) && (bulletXplus <= enemyXplus) = True
                                 | (bulletYmin <= enemyYplus) && (bulletYmin >= enemyYmin) && (bulletXplus >= enemyXmin) && (bulletXplus <= enemyXplus) = True
                                 | otherwise = False                                                                                                                                                                                        
        where   bulletYplus = snd (bulletPosition bullet) +2
                bulletYmin = snd (bulletPosition bullet) -2
                enemyYplus = snd (enemyPosition enemy) +10
                enemyYmin = snd (enemyPosition enemy) -10
                enemyXplus = fst (enemyPosition enemy) +10
                enemyXmin = fst (enemyPosition enemy) -10
                bulletXplus = fst (bulletPosition bullet) +4

-- Score and Dead Enemies
checkDead :: Game -> Game
checkDead game = game {bullets = bulletList, enemies = enemyList, score = newScore}
    where newScore = calcScore (enemies game) (score game)
          bulletList = finalBulletList (bullets game)
          enemyList  = finalEnemyList  (enemies game)
          
calcScore :: [Enemy] -> Int -> Int
calcScore [] score = score
calcScore [enemy] score = case enemyState enemy of
    Alive -> score
    Dead -> 100 + score
calcScore (enemy:rest) score = case enemyState enemy of
    Alive -> calcScore rest score
    Dead -> 100 + calcScore rest score

finalBulletList :: [Bullet] -> [Bullet]
finalBulletList [] = []
finalBulletList [bullet] | not (bulletCollide bullet) = [bullet]
                         | otherwise = []
finalBulletList (bullet:rest) | not (bulletCollide bullet) = bullet : finalBulletList rest
                              | otherwise = finalBulletList rest

finalEnemyList :: [Enemy] -> [Enemy]
finalEnemyList [] = []
finalEnemyList [enemy] | x < (-640) = []
                       | otherwise = case enemyState enemy of
        Alive -> [enemy]
        Dead -> []
    where x = fst (enemyPosition enemy)
finalEnemyList (enemy:rest) = case enemyState enemy of
    Alive -> enemy : finalEnemyList rest
    Dead -> finalEnemyList rest

checkSpawn :: Game -> Game
checkSpawn game = game {spawnRate = newSpawnRate}
    where newSpawnRate = calcSpawnRate (spawnRate game) (score game)

calcSpawnRate :: Float -> Int -> Float
calcSpawnRate _ 0 = 3
calcSpawnRate _ 500  = 2.5
calcSpawnRate _ 1000 = 2
calcSpawnRate _ 1500 = 1.75
calcSpawnRate _ 2000 = 1.5
calcSpawnRate _ 2500 = 1.25
calcSpawnRate _ 3000 = 0.875
calcSpawnRate _ 3500 = 0.75
calcSpawnRate _ 4000 = 0.625
calcSpawnRate _ 4500 = 0.5
calcSpawnRate _ 5000 = 0.375
calcSpawnRate _ 5500 = 0.25
calcSpawnRate _ 6000 = 0.125
calcSpawnRate _ 7500 = 0.1
calcSpawnRate spawnRate _ = spawnRate