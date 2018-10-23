module State where

import Enteties

data Game = Game {
    player :: Player
} deriving Show

initialState :: Game
initialState = Game {
    player {Alive, baseHealth, startingPoint, basePowerUp}    
}
