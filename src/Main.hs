module Main where

import State 
import Controller
import View
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO window background fps initialState render input step