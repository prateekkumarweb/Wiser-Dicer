module Logic where

import Data.Array
import Data.Foldable ( asum )

import Game
import Graphics.Gloss.Interface.Pure.Game

-- isCoordCorrect = inRange ((0, 0), (n - 1, n - 1))

-- switchPlayer game =
--     case gamePlayer game of
--       PlayerX -> game { gamePlayer = PlayerO }
--       PlayerO -> game { gamePlayer = PlayerX }

-- full :: [Cell] -> Maybe Player
-- full (cell@(Just player):cells) | all (== cell) cells = Just player
-- full _                                                = Nothing

-- winner :: Board -> Maybe Player
-- winner board = asum $ map full $ rows ++ cols ++ diags
--     where rows  = [[board ! (i,j) | i <- [0..n-1]] | j <- [0..n-1]]
--           cols  = [[board ! (j,i) | i <- [0..n-1]] | j <- [0..n-1]]
--           diags = [[board ! (i,i) | i <- [0..n-1]]
--                   ,[board ! (i,j) | i <- [0..n-1], let j = n-1-i ]]

-- countCells :: Cell -> Board -> Int
-- countCells cell = length . filter ((==) cell) . elems

-- checkGameOver game
--     | Just p <- winner board =
--         game { gameState = GameOver $ Just p }
--     | countCells Nothing board == 0 =
--         game { gameState = GameOver Nothing }
--     | otherwise = game
--     where board = gameBoard game

-- playerTurn :: Game -> (Int, Int) -> Game
-- playerTurn game cellCoord
--     | isCoordCorrect cellCoord && board ! cellCoord == Nothing =
--         checkGameOver
--         $ switchPlayer
--         $ game { gameBoard = board // [(cellCoord, Just player)] }
--     | otherwise = game
--     where board = gameBoard game
--           player = gamePlayer game

moveConfigPlayerUp :: ConfigPlayer -> ConfigPlayer
moveConfigPlayerUp  plyr = ConfigPlayer{
	top = south plyr,
	bottom = north plyr,
	north = top plyr,
	south = bottom plyr,
	east =east plyr,
	west = west plyr,
}

moveConfigPlayerDown :: ConfigPlayer -> ConfigPlayer
moveConfigPlayerDown  plyr = ConfigPlayer{
	top = north plyr,
	bottom = south plyr,
	north = bottom plyr,
	south = top plyr,
	east =east plyr,
	west = west plyr,
}

moveConfigPlayerRight :: ConfigPlayer -> ConfigPlayer
moveConfigPlayerRight  plyr = ConfigPlayer{
	top = east plyr,
	bottom = west plyr,
	north = north plyr,
	south = south plyr,
	east = bottom plyr,
	west = top plyr,
}

moveConfigPlayerLeft :: ConfigPlayer -> ConfigPlayer
moveConfigPlayerLeft  plyr = ConfigPlayer{
	top = west plyr,
	bottom = east plyr,
	north = north plyr,
	south = south plyr,
	east = top plyr,
	west = bottom plyr,
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
checkNewCoord ::Int -> Int -> Board -> (Int,Int) -> Bool
checkNewCoord i j board (x,y) = elem (x+i,y+j) $ allReachableCoords board
checkMove :: Int -> Int-> Game -> Bool
checkMove i j game = checkNewCoord i j (gameBoard game) $ findCoordsPlayer $ gameBoard game

checkAndMove :: Int -> Int -> Game -> Game
checkAndMove i j game 
    | checkMove i j game = game { gameBoard = (gameBoard game) // [((x,y),Empty),((x+i,y+j),Player)] }
    | otherwise = game
    where
      (x,y) = findCoordsPlayer $ gameBoard game

movePlayer :: Game -> Key -> Game
movePlayer game key = 
    case key of
      (SpecialKey KeyUp) -> checkAndMove 0 1 game 
      (SpecialKey KeyDown) -> checkAndMove 0 (-1) game 
      (SpecialKey KeyRight) -> checkAndMove 1 0 game 
      (SpecialKey KeyLeft) -> checkAndMove (-1) 0 game 
      otherwise -> game


transformGame (EventKey key Up _ _) game =
    case gameState game of
      Running -> movePlayer game key
      GameOver -> initialGame
transformGame _ game = game