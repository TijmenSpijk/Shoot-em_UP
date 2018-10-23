module Main where

import Graphics.Gloss

window :: Display
window = InWindow "Shoot'em Up" (400, 400) (10, 10)

background :: Color
background = white

drawing :: Picture
drawing = circle 80

main :: IO ()
main = display window background drawing
              


