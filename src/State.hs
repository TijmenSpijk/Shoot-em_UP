module State where

data Game = Game {
    playerLocation :: (Float, Float),
    playerMovement :: (Float, Float)
} deriving Show

initialState :: Game
initialState = Game {
    playerLocation = (-100, -100),
    playerMovement = (0, 0)  
}
