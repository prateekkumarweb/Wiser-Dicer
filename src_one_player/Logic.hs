module Logic where

import Data.Array
import Data.Foldable ( asum )
import Helper
import Game
import Graphics.Gloss.Interface.Pure.Game

moveConfigPlayerUp :: ConfigPlayer -> ConfigPlayer
moveConfigPlayerUp  plyr = ConfigPlayer{
	top = south plyr,
	bottom = north plyr,
	north = top plyr,
	south = bottom plyr,
	east =east plyr,
	west = west plyr
}

moveConfigPlayerDown :: ConfigPlayer -> ConfigPlayer
moveConfigPlayerDown  plyr = ConfigPlayer{
	top = north plyr,
	bottom = south plyr,
	north = bottom plyr,
	south = top plyr,
	east =east plyr,
	west = west plyr
}

moveConfigPlayerRight :: ConfigPlayer -> ConfigPlayer
moveConfigPlayerRight  plyr = ConfigPlayer{
	top = west plyr,
	bottom = east plyr,
	north = north plyr,
	south = south plyr,
	east = top plyr,
	west = bottom plyr
}

moveConfigPlayerLeft :: ConfigPlayer -> ConfigPlayer
moveConfigPlayerLeft  plyr = ConfigPlayer{
	top = east plyr,
	bottom = west plyr,
	north = north plyr,
	south = south plyr,
	east = bottom plyr,
	west = top plyr
}

allReachableCoords :: Board -> [(Int,Int)]
allReachableCoords board = map fst $ filter (\(_, e) -> e /= Wall)  (assocs board) 
findCoordsPlayer :: Board -> (Int,Int)
findCoordsPlayer board = fst $ ( filter (\(_, e) -> e == Player)  (assocs board) ) !! 0
-- i=0, j=1 move Up
-- i=0, j=-1 move Down
-- i=1, j=0 move Right
-- i=-1, j=0 move left

-- coord can be on empty, target or player 
moveConfigPlayer :: Int -> Int ->ConfigPlayer -> ConfigPlayer
moveConfigPlayer i j plyr = 
	case (i,j) of
		(0,1) -> moveConfigPlayerUp plyr 
		(0,-1) -> moveConfigPlayerDown plyr 
		(1,0) -> moveConfigPlayerRight plyr 
		(-1,0) -> moveConfigPlayerLeft plyr 
		otherwise -> plyr

checkNewCoord ::Int -> Int -> Board -> (Int,Int) -> Bool
checkNewCoord i j board (x,y) = elem (x+i,y+j) $ allReachableCoords board
checkMove :: Int -> Int-> Game -> Bool
checkMove i j game = checkNewCoord i j (gameBoard game) $ findCoordsPlayer $ gameBoard game

checkAndMove :: Int -> Int -> Game -> Game
checkAndMove i j game 
    | checkMove i j game = game { gameBoard = (gameBoard game) // [((x,y),Empty),((x+i,y+j),Player)] ,
    							configPlayer = moveConfigPlayer i j (configPlayer game),
    							numberOfMoves = (numberOfMoves game) + 1
    							}

    | otherwise = game
    where
      (x,y) = findCoordsPlayer $ gameBoard game

movePlayer :: Game -> Key -> Game
movePlayer game key = 
    case key of
      (SpecialKey KeyEnter) -> changeGame $ (level game) + 1
      (SpecialKey KeyUp) -> checkAndMove 0 1 game 
      (SpecialKey KeyDown) -> checkAndMove 0 (-1) game 
      (SpecialKey KeyRight) -> checkAndMove 1 0 game 
      (SpecialKey KeyLeft) -> checkAndMove (-1) 0 game 
      otherwise -> game

checkVictory :: Game -> Game
checkVictory game 
	| playerCoords == targetCoords && topI == targetI  = game { gameState = GameOver }
	| otherwise = game
	where
		playerCoords = findCoordsPlayer $ gameBoard game
		targetCoords = fst $ finalTarget game
		topI = top (configPlayer game)
		targetI = snd ( finalTarget game )



changeGame :: Int -> Game
changeGame i = initGameForMapLevel i 

startNewGame :: Game -> Key -> Game
startNewGame game key 
	| key == (SpecialKey KeyShiftL) = changeGame $ (level game) + 1
	| key == (SpecialKey KeySpace) = changeGame $ (level game) + 1
	| otherwise = game


transformGame (EventKey key Up _ _) game =
    case gameState game of
      Running -> checkVictory $ movePlayer game key
      GameOver -> startNewGame game key
transformGame _ game = game