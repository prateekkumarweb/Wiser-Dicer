
module Main where

import Data.Array
import Graphics.Gloss
import Graphics.Gloss.Data.Color

sizeX = floor (gridSize * n+ lineWidth)
sizeY = floor (gridSize * n + lineWidth) 
gridSize = 100
lineWidth = 5
n = 5

tileSize :: Float
tileSize = 100.0

initialGame = blank

makeTile :: Float -> Float -> Picture
makeTile x y = pictures [
		line[ (x , y) , (x , y + tileSize)],
		line[ (x , y) , (x + tileSize , y )],
		line[ (x + tileSize , y) , (x + tileSize , y + tileSize)],
		line[ (x , y + tileSize) , (x + tileSize , y + tileSize)]
	]

gameGrid :: Picture
gameGrid = pictures [
		-- makes a grid of n x n tile
		( makeTile x y ) | x <- [0 , tileSize.. (n-1) * tileSize ] , y <- [0 , tileSize.. (n-1) * tileSize ]  
	]


gameAsPicture :: Picture -> Picture
gameAsPicture p = translate (fromIntegral sizeX * (-0.5))
                               (fromIntegral sizeY * (-0.5))
                               gameGrid

transformGame _ game = game

main :: IO ()
main = play window backgroundColor 30 initialGame gameAsPicture transformGame (const id)
 where 
 	window = InWindow "popl" (sizeX, sizeY) (100, 100)
	backgroundColor = makeColor 255 255 255 128
	