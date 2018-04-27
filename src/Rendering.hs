module Rendering where

import Data.Array
import Game
import Graphics.Gloss
import Graphics.Gloss.Data.Color

-- adds finishing point at given block number ( can use maybe )
makeFinish :: Int -> Int -> Picture
makeFinish xBlock yBlock = 
	pictures [
	makeWall xBlock yBlock green,
	translate ((fromIntegral xBlock) * tileSize + 25.0) ((fromIntegral yBlock) * tileSize + 25.0) $ scale 0.5 0.5 $ text "3"
	]

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
		makeWall x y (orange),
		wallGrid ls
	] 

-- used to make a colored tile ( can be used for making any type of tile) 
makeWall :: Int -> Int -> Color-> Picture
makeWall xBlock yBlock icolor = color icolor ( polygon [(x,y),(x,y + tileSize),(x + tileSize,y + tileSize),(x + tileSize,y)])
	where 
		x = (fromIntegral xBlock) * tileSize 
		y = (fromIntegral yBlock) * tileSize 

gameGridIntial :: Picture
gameGridIntial = pictures $ [
		-- makes a grid of n x n tile
		( makeTile x y ) | x <- [0 , tileSize.. (nF-1) * tileSize ] , y <- [0 , tileSize.. (nF-1) * tileSize ]  
	] 
	where
		wallPos = findAllAnything map0 (nI-1)

cellsOfBoard :: Board -> Cell -> Picture -> Picture
cellsOfBoard board cell cellPicture =
    pictures
    $ map (snapPictureToCell cellPicture . fst)
    $ filter (\(_, e) -> e == cell)
    $ assocs board

-- change name of this function
snapPictureToCell :: Picture -> (Int, Int) -> Picture
snapPictureToCell cellPic (xBlock,yBlock) = translate x y cellPic
	where 
		x = (fromIntegral xBlock) * tileSize
		y = (fromIntegral yBlock) * tileSize  

cellPicPlayer :: ConfigPlayer -> Picture
cellPicPlayer player = pictures [
		makeWall 0 0 white,
		translate (25.0) (25.0) $ scale 0.5 0.5 $ text topPlayer,
		color (greyN 0.5) $ translate (-tileSize + 25.0) (25.0) $ scale 0.25 0.25 $ text eastPlayer,
		color (greyN 0.5) $ translate (tileSize + 25.0) (25.0) $ scale 0.25 0.25 $ text westPlayer,
		color (greyN 0.5) $ translate (25.0) (25.0 + tileSize) $ scale 0.25 0.25 $ text northPlayer,
		color (greyN 0.5) $ translate (25.0) (25.0 - tileSize) $ scale 0.25 0.25 $ text southPlayer
	]	
	where
		topPlayer = show $ top player
		eastPlayer = show $ east player
		westPlayer = show $ west player
		southPlayer = show $ south player
		northPlayer = show $ north player


cellPicTarget :: Int -> Picture
cellPicTarget targetInt = pictures [
		makeWall 0 0 green,
		translate (25.0) (25.0) $ scale 0.5 0.5 $ text tInt
	]	
	where
		tInt = show targetInt

cellPicWall :: Picture
cellPicWall = pictures [
		makeWall 0 0 $ makeColorI 139 71 38 255
	]	

cellPicEmpty :: Picture
cellPicEmpty  = pictures [
		makeWall 0 0 $ makeColorI 224 238 224 255
	]	

gameGrid :: Game -> Picture
gameGrid game = pictures [
		cellsOfBoard board Empty cellPicEmpty,
		cellsOfBoard board Wall cellPicWall,
		cellsOfBoard board Target $ cellPicTarget (finalTarget game),
		cellsOfBoard board Player $ cellPicPlayer (configPlayer game),
		gameGridIntial
	]
	where
		board = gameBoard game

-- change the names above
gameAsPicture :: Game -> Picture
gameAsPicture game = translate (fromIntegral sizeX * (-0.5))
                               (fromIntegral sizeY * (-0.5))
                               frame 
                	where
                        	frame = gameGrid game  	   