module Main where

import Data.Array
import Graphics.Gloss
import Graphics.Gloss.Data.Color

sizeX = floor (gridSize * nF+ lineWidth)
sizeY = floor (gridSize * nF + lineWidth) 
gridSize = 100.0
lineWidth = 5.0

nI :: Int
nI = 5
nF :: Float
nF = 5.0

tileSize :: Float
tileSize = 100.0

initialGame = blank

map0 :: [String]
map0= [
	 "bbbbb",
	 "eeeeb",
	 "bbbbb",
	 "beeee",
	 "bbbbb"
	]

findB :: [Char] -> Int -> [Int]
findB [] _ = []
findB (x:xs) i 
 | x == 'b' = i : findB  xs (i+1)
 | otherwise = findB xs (i+1)

findAllB :: [String] -> Int -> [(Int,Int)]
findAllB [] _ = []
findAllB  (x:xs) i = row ++ (findAllB xs (i-1)) 
 	where
 		row = [ (b,a) | b<- findB x 0, let a = i ]

-- adds finishing point at given block number ( can use maybe )
makeFinish :: Int -> Int -> Picture
makeFinish xBlock yBlock = translate ((fromIntegral xBlock) * tileSize + 25.0) ((fromIntegral yBlock) * tileSize + 25.0) $ scale 0.5 0.5 $ text "3"

makeTile :: Float -> Float -> Picture
makeTile x y = pictures [
		line[ (x , y) , (x , y + tileSize)],
		line[ (x , y) , (x + tileSize , y )],
		line[ (x + tileSize , y) , (x + tileSize , y + tileSize)],
		line[ (x , y + tileSize) , (x + tileSize , y + tileSize)]
	]

-- Use maybe as a list might not always give a Picture
wallGrid :: [(Int , Int)] -> Picture
wallGrid [] = blank
wallGrid ((x,y) : ls) = pictures [
		makeWall x y,
		wallGrid ls
	] 

-- used to make a colored tile ( can be used for making any type of tile) 
makeWall :: Int -> Int -> Picture
makeWall xBlock yBlock = color orange ( polygon [(x,y),(x,y + tileSize),(x + tileSize,y + tileSize),(x + tileSize,y)])
	where 
		x = (fromIntegral xBlock) * tileSize 
		y = (fromIntegral yBlock) * tileSize 

gameGrid :: Picture
gameGrid = pictures $ [
		-- makes a grid of n x n tile
		( makeTile x y ) | x <- [0 , tileSize.. (nF-1) * tileSize ] , y <- [0 , tileSize.. (nF-1) * tileSize ]  
	] ++ [ makeFinish 0 1 ] ++ [ wallGrid  wallPos]
	where
		wallPos = findAllB map0 (nI-1)


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
	