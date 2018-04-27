module Main where

import Data.Array
import Graphics.Gloss
import Graphics.Gloss.Data.Color

import Game
import Rendering
import Logic

-- transformGame _ game = game

main :: IO ()
main = play window backgroundColor 30 initialGame gameAsPicture transformGame (const id)
 where 
 	window = InWindow "popl" (sizeX, sizeY) (100, 100)
	backgroundColor = cyan
	
