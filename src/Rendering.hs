module Rendering where

import Data.Array
import Game
import Maps
import Helper
import Graphics.Gloss
import Graphics.Gloss.Data.Color

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

gameGridIntial :: Int -> Picture
gameGridIntial i= pictures $ [
		-- makes a grid of n x n tile
		( makeTile x y ) | x <- [0 , tileSize.. (nF-1) * tileSize ] , y <- [0 , tileSize.. (nF-1) * tileSize ]  
	] 
	where
		-- wallPos = findAllAnything map0 (n-1)
		-- map0 = (lMap $ maps !! i)
		n = (nI $ maps !! i)
		nF = fromIntegral n


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
		translate tf tf $ scale (0.5*tileSize/100) (0.5*tileSize/100) $ text topPlayer,
		color (greyN 0.5) $ translate (-tileSize + tf) (tf) $ scale tf2 tf2 $ text eastPlayer,
		color (greyN 0.5) $ translate (tileSize + tf) (tf) $ scale tf2 tf2 $ text westPlayer,
		color (greyN 0.5) $ translate (tf) (tf + tileSize) $ scale tf2 tf2 $ text northPlayer,
		color (greyN 0.5) $ translate (tf) (tf - tileSize) $ scale tf2 tf2 $ text southPlayer
	]	
	where
		topPlayer = show $ top player
		eastPlayer = show $ east player
		westPlayer = show $ west player
		southPlayer = show $ south player
		northPlayer = show $ north player
		tf = (25.0*tileSize/100)
		tf2 = (0.25*tileSize/100)


cellPicTarget :: Int -> Picture
cellPicTarget targetInt = pictures [
		makeWall 0 0 green,
		translate (25.0*mf) (25.0*mf) $ scale (0.5*mf) (0.5*mf) $ text tInt
	]	
	where
		tInt = show targetInt
		mf = tileSize/100

cellPicWall :: Picture
cellPicWall = pictures [
		makeWall 0 0 $ makeColorI 139 71 38 255
	]	

cellPicEmpty :: Picture
cellPicEmpty  = pictures [
		makeWall 0 0 $ makeColorI 224 238 224 255
	]	

gameGrid :: Int ->Game -> Picture
gameGrid i game = pictures [
		scoreboard i game,
		cellsOfBoard board Empty cellPicEmpty,
		cellsOfBoard board Wall cellPicWall,
		snapPictureToCell ( cellPicTarget (snd (finalTarget game)) ) $ fst (finalTarget game),
		cellsOfBoard board Player $ cellPicPlayer (playerConfigPlayer i game),
		gameGridIntial (level game)
	]
	where
		board = playerBoard i game

------------changed
scoreboard :: Int -> Game ->Picture
scoreboard i game = pictures [translate (2*tileSize) (-tileSize) $ scale 0.5 0.5 $ text $ "Total Moves" ++ (show $ playerNumOfMoves i game) ]

makeFinal :: Game -> Picture -> Picture
makeFinal game pic 
	| (gameState game) == GameOver = pictures [ pic, translate (-tileSize) (-tileSize) $ scale (0.5*tileSize/100) (0.5*tileSize/100) $ text "You Won"]
	| otherwise = pic

playerBoard :: Int -> Game -> Board
playerBoard i game
	| i == 0 = fst (gameBoard game)
	| otherwise = snd (gameBoard game)

playerConfigPlayer :: Int -> Game -> ConfigPlayer
playerConfigPlayer i game
	| i == 0 = fst (configPlayer game)
	| otherwise = snd (configPlayer game)  

playerNumOfMoves :: Int -> Game -> Int
playerNumOfMoves i game
	| i == 0 = fst (numberOfMoves game)
	| otherwise = snd (numberOfMoves game)  


-- change the names above
gameAsPicture :: Game -> Picture
gameAsPicture game = pictures [ 
					  translate (fromIntegral(-n-1)*tileSize) ((-1)*fromIntegral(n)*tileSize/2) frame1
					,translate (tileSize) ((-1)*fromIntegral(n+1)*tileSize/2) frame2
					]
                	where
                        	frame1 = makeFinal game $ gameGrid 0 game
                        	frame2 = makeFinal game $ gameGrid 1 game
                        	n = nI $ maps !! (level game) 

