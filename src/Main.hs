module Main where

import State 
import Controller
import View
import Graphics.Gloss

main :: IO ()
main = play window background fps initialState render handleKeys movePlayer