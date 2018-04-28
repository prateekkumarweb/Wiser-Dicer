module Main where

import Data.Array
import Graphics.Gloss
import Graphics.Gloss.Data.Color

import Game
import Rendering
import Logic

-- | Main function that controls the state of the game TransformGame _ game = game

main :: IO ()
main = play window backgroundColor 30 initialGame gameAsPicture transformGame (const id)
 where 
 	window = FullScreen
	backgroundColor = cyan
