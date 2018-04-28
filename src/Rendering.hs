module Rendering where

import Data.Array
import Game
import Maps
import Helper
import Graphics.Gloss
import Graphics.Gloss.Data.Color

        
-- | This function is used for making grid and tile at any given point x,y
makeTile :: Float -> Float -> Picture
makeTile x y = pictures [
		line[ (x , y) , (x , y + tileSize)],
		line[ (x , y) , (x + tileSize , y )],
		line[ (x + tileSize , y) , (x + tileSize , y + tileSize)],
		line[ (x , y + tileSize) , (x + tileSize , y + tileSize)]
	]

-- | This function  returns the picture that contains the wall cell
wallGrid :: [(Int , Int)] -> Picture
wallGrid [] = blank
wallGrid ((x,y) : ls) = pictures [
		makeWall x y (orange),
		wallGrid ls
	] 

-- | used to make a colored wall tile
makeWall :: Int -> Int -> Color-> Picture
makeWall xBlock yBlock icolor = color icolor ( polygon [(x,y),(x,y + tileSize),(x + tileSize,y + tileSize),(x + tileSize,y)])
	where 
		x = (fromIntegral xBlock) * tileSize 
		y = (fromIntegral yBlock) * tileSize 

-- | Takes level of the game as input and returns the picture containing tiles
gameGridIntial :: Int -> Picture
gameGridIntial i= pictures $ [
		-- makes a grid of n x n tile
		( makeTile x y ) | x <- [0 , tileSize.. (nF-1) * tileSize ] , y <- [0 , tileSize.. (nF-1) * tileSize ]  
	] 
	where
		n = (nI $ maps !! i)
		nF = fromIntegral n

-- | Takes Board cell and picture as input and combines them and returned the combined picture
cellsOfBoard :: Board -> Cell -> Picture -> Picture
cellsOfBoard board cell cellPicture =
    pictures
    $ map (snapPictureToCell cellPicture . fst)
    $ filter (\(_, e) -> e == cell)
    $ assocs board

-- | Maps each cell to the board in picture and returns the picture
snapPictureToCell :: Picture -> (Int, Int) -> Picture
snapPictureToCell cellPic (xBlock,yBlock) = translate x y cellPic
	where 
		x = (fromIntegral xBlock) * tileSize
		y = (fromIntegral yBlock) * tileSize  



-- | Takes the configuration of the player as input and displays it on the screen in the form of picture
cellPicPlayer :: ConfigPlayer -> Picture
cellPicPlayer player = pictures [
		makeWall 0 0 white,
		translate tf tf $ scale (0.75*tileSize/100) (0.75*tileSize/100) $ topPlayer,
		color (greyN 0.5) $ translate (-tileSize/3 ) (tileSize/2.5) $ scale tf2 tf2 $  westPlayer,
		color (greyN 0.5) $ translate (tileSize + tileSize/6) (tileSize/2.5) $ scale tf2 tf2 $  eastPlayer,
		color (greyN 0.5) $ translate (tileSize/2.5) (tileSize/6 + tileSize) $ scale tf2 tf2 $  northPlayer,
		color (greyN 0.5) $ translate (tileSize/2.5) (-tileSize/3) $ scale tf2 tf2 $  southPlayer
	]	
	where
		topPlayer = makeI $ top player
		eastPlayer = makeI $ east player
		westPlayer = makeI $ west player
		southPlayer = makeI $ south player
		northPlayer = makeI $ north player
		tf = (15.0*tileSize/100)
		tf2 = (0.25*tileSize/100)



-- | colors and make the target cell 
cellPicTarget :: Int -> Picture
cellPicTarget targetInt = pictures [
		makeWall 0 0 green,
		translate (15.0*mf) (15.0*mf) $ scale (0.75*mf) (0.75*mf) $ makeI tInt
	]	
	where
		tInt = targetInt
		mf = tileSize/100


-- | colors and make the wall cell
cellPicWall :: Picture
cellPicWall = pictures [
		makeWall 0 0 $ makeColorI 139 71 38 255
	]	


-- | Colors the empty cell
cellPicEmpty :: Picture
cellPicEmpty  = pictures [
		makeWall 0 0 $ makeColorI 224 238 224 255
	]	


-- | Combines all the pictures and generates the rendered image on the screen
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

-- | Prints the number of moves of both the plaer on the screen
scoreboard :: Int -> Game ->Picture
scoreboard i game = pictures [translate (1.5*tileSize) (-tileSize) $ scale 0.2 0.2 $ text $ "Total Moves" ++ (show $ playerNumOfMoves i game) ]


-- | Returns the picture of the player score who has won the game
makeFinal :: Game -> Picture -> Picture
makeFinal game pic 
	| (gameState game) == GameOver = pictures [ pic, whoWon game]
	| otherwise = pic

-- | Finds the player who won the game
whoWon :: Game -> Picture
whoWon game 
		| (isPlaying game) == Player0 = translate (-tileSize) (-tileSize) $ scale (0.2*tileSize/100) (0.2*tileSize/100) $ text "Winner Player2"
		| otherwise =  translate (-tileSize) (-tileSize) $ scale (0.2*tileSize/100) (0.2*tileSize/100) $ text "Winner Player1"



-- | Returns the number of moves of player
playerBoard :: Int -- ^ o for player0 1 for player1
            -> Game -- ^ game at any given time
            -> Board -- ^ returned board of player
playerBoard i game
	| i == 0 = fst (gameBoard game)
	| otherwise = snd (gameBoard game)


-- | Returns the number of moves of player
playerConfigPlayer :: Int -- ^ o for player0 1 for player1
                   -> Game -- ^ game at any given time
                   -> ConfigPlayer  -- ^ returned configuration of player
playerConfigPlayer i game
	| i == 0 = fst (configPlayer game)
	| otherwise = snd (configPlayer game)  


-- | Returns the number of moves of player
playerNumOfMoves :: Int   -- ^ o for player0 1 for player1 
				 -> Game  -- ^ game at any given time
				 -> Int   -- ^ returned number of moves
playerNumOfMoves i game
	| i == 0 = fst (numberOfMoves game)
	| otherwise = snd (numberOfMoves game)  


-- | converts given integer into picture with same number of dots
makeI :: Int -> Picture
makeI i =
	case i of
		1 -> makeOne
		2 -> makeTwo
		3 -> makeThree
		4 -> makeFour
		5 -> makeFive
		6 -> makeSix


-- | MAkes a Picture of one dot to represents the dice(1)
makeOne :: Picture 
makeOne = pictures[
        translate (tileSize/2) (tileSize/2) $ circleSolid radius
        ]
        where
            radius = tileSize/10
            
-- | MAkes a Picture of 2 dots to represents the dice(2)
makeTwo :: Picture
makeTwo = pictures[
        translate (tileSize/3) (2*tileSize/3) $ circleSolid radius,
        translate (2*tileSize/3) (tileSize/3) $ circleSolid radius
        ]
        where
            radius = tileSize/10            

-- | MAkes a Picture of 3 dots to represents the dice(3)
makeThree :: Picture 
makeThree = pictures[
        translate (tileSize/4) (3*tileSize/4) $ circleSolid radius,
        translate (3*tileSize/4) (tileSize/4) $ circleSolid radius,
        translate (tileSize/2) (tileSize/2) $ circleSolid radius
        ]
        where
            radius = tileSize/10        
-- | MAkes a Picture of 4 dots to represents the dice(4)
makeFour :: Picture
makeFour  = pictures[
        translate (tileSize/4) (3*tileSize/4) $ circleSolid radius,
        translate (3*tileSize/4) (tileSize/4) $ circleSolid radius,
        translate (3*tileSize/4) (3*tileSize/4) $ circleSolid radius,
        translate (tileSize/4) (tileSize/4) $ circleSolid radius
        ]
        where
            radius = tileSize/10 

-- | MAkes a Picture of 5 dots to represents the dice(5)
makeFive :: Picture
makeFive  = pictures[
        translate (tileSize/4) (3*tileSize/4) $ circleSolid radius,
        translate (3*tileSize/4) (tileSize/4) $ circleSolid radius,
        translate (3*tileSize/4) (3*tileSize/4) $ circleSolid radius,
        translate (tileSize/4) (tileSize/4) $ circleSolid radius,
        translate (tileSize/2) (tileSize/2) $ circleSolid radius
        ]
        where
            radius = tileSize/10 


-- | MAkes a Picture of 6 dots to represents the dice(6)
makeSix :: Picture
makeSix  = pictures[
        translate (tileSize/4) (2*tileSize/3) $ circleSolid radius,
        translate (tileSize/4) (tileSize/3) $ circleSolid radius,
        translate (2*tileSize/4) (2*tileSize/3) $ circleSolid radius,
        translate (2*tileSize/4) (tileSize/3) $ circleSolid radius,
        translate (3*tileSize/4) (2*tileSize/3) $ circleSolid radius,
        translate (3*tileSize/4) (tileSize/3) $ circleSolid radius
        ]
        where
            radius = tileSize/10

-- | Convets the given game into picture and dislpays it in the form of the picture
gameAsPicture :: Game -> Picture
gameAsPicture game = pictures [ line [(0,-1000),(0,1000)] 
					 , translate (fromIntegral(-n-1)*tileSize) ((-1)*fromIntegral(n)*tileSize/2) frame1
					,translate (tileSize/2) ((-1)*fromIntegral(n)*tileSize/2) frame2
					]
                	where
                        	frame1 = makeFinal game $ gameGrid 0 game
                        	frame2 = makeFinal game $ gameGrid 1 game
                        	n = nI $ maps !! (level game) 
