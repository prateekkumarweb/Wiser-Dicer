module Logic where

import Data.Array
import Data.Foldable ( asum )
import Helper
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


-- | If the player choses to move the dice in north direction then congiuration changes
moveConfigPlayerUp :: ConfigPlayer -> ConfigPlayer
moveConfigPlayerUp  plyr = ConfigPlayer{
	top = south plyr,
	bottom = north plyr,
	north = top plyr,
	south = bottom plyr,
	east =east plyr,
	west = west plyr
}


-- | If the player choses to move the dice in south direction then congiuration changes
moveConfigPlayerDown :: ConfigPlayer -> ConfigPlayer
moveConfigPlayerDown  plyr = ConfigPlayer{
	top = north plyr,
	bottom = south plyr,
	north = bottom plyr,
	south = top plyr,
	east =east plyr,
	west = west plyr
}

-- | If the player choses to move the dice in east direction then congiuration changes
moveConfigPlayerRight :: ConfigPlayer -> ConfigPlayer
moveConfigPlayerRight  plyr = ConfigPlayer{
	top = east plyr,
	bottom = west plyr,
	north = north plyr,
	south = south plyr,
	east = bottom plyr,
	west = top plyr
}


-- | If the player choses to move the dice in west direction then congiuration changes
moveConfigPlayerLeft :: ConfigPlayer -> ConfigPlayer
moveConfigPlayerLeft  plyr = ConfigPlayer{
	top = west plyr,
	bottom = east plyr,
	north = north plyr,
	south = south plyr,
	east = top plyr,
	west = bottom plyr
}


-- | Finds all the coordinates that are reachable
allReachableCoords :: Board -> [(Int,Int)]
allReachableCoords board = map fst $ filter (\(_, e) -> e /= Wall)  (assocs board) 


-- | Finds the coordiates of the player in the given board
findCoordsPlayer :: Board -> (Int,Int)
findCoordsPlayer board = fst $ ( filter (\(_, e) -> e == Player)  (assocs board) ) !! 0
-- i=0, j=1 move Up
-- i=0, j=-1 move Down
-- i=1, j=0 move Right
-- i=-1, j=0 move left

-- | coord can be on empty, target or player 
moveConfigPlayer :: Int -> Int ->ConfigPlayer -> ConfigPlayer
moveConfigPlayer i j plyr = 
	case (i,j) of
		(0,1) -> moveConfigPlayerUp plyr 
		(0,-1) -> moveConfigPlayerDown plyr 
		(1,0) -> moveConfigPlayerRight plyr 
		(-1,0) -> moveConfigPlayerLeft plyr 
		otherwise -> plyr

-- | Checks the new coordinate of the player after the move whether it is valid or not 
checkNewCoord ::Int -> Int -> Board -> (Int,Int) -> Bool
checkNewCoord i j board (x,y) = elem (x+i,y+j) $ allReachableCoords board


-- | Checks the move of the player and returns the new configuration based on checknewMoves function
checkMove :: Int -> Int-> Game -> Bool
checkMove i j game = checkNewCoord i j (playerToMoveBoard game) $ findCoordsPlayer $ playerToMoveBoard game
checkAndMove :: Int -> Int -> Game -> Game
checkAndMove i j game 
    | checkMove i j game = game { 
    							gameBoard = newGameBoard i j game,
    							configPlayer = newConfigPlayer i j game,
    							numberOfMoves = newNumberOfMoves game,
    							isPlaying = newIsPlaying game
    						}

    | otherwise = game

-- | Returns the (Board,Board) according to the move by the playing player
newGameBoard :: Int -> Int -> Game -> (Board, Board)
newGameBoard i j game 
	| (isPlaying game) == Player0 = ((fst (gameBoard game)) // [((x1,y1),Empty),((x1+i,y1+j),Player)], snd (gameBoard game))
	| otherwise = (fst(gameBoard game),(snd (gameBoard game)) // [((x2,y2),Empty),((x2+i,y2+j),Player)])
	where
		(x1,y1) = findCoordsPlayer $ fst (gameBoard game)
		(x2,y2) = findCoordsPlayer $ snd (gameBoard  game)


-- | Returns the (ConfigPlayer,ConfigPlayer) according to the move by the playing player
newConfigPlayer :: Int -> Int-> Game -> (ConfigPlayer, ConfigPlayer)
newConfigPlayer i j game 
	| (isPlaying game) == Player0  = ((moveConfigPlayer i j (fst (configPlayer game))),(snd (configPlayer game)))
	| otherwise = ((fst (configPlayer game) ), moveConfigPlayer i j (snd(configPlayer game)) )


-- | Returns the (Int,Int) updating the number of moves of the current playing player
newNumberOfMoves :: Game -> (Int,Int)
newNumberOfMoves game 
	| (isPlaying game) == Player0  = ((fst(numberOfMoves game)+1),snd(numberOfMoves game))
	| otherwise = (fst(numberOfMoves game),(snd(numberOfMoves game)+1) )


-- | Returns the Turn of player who is [laying]
newIsPlaying :: Game -> Turn
newIsPlaying game 
	| (isPlaying game) == Player0  = Player1
	| otherwise = Player0

-- | Returns the number of moves of the current playing player
playerToMoveNum :: Game -> Int
playerToMoveNum game
	| (isPlaying game) == Player0 = fst (numberOfMoves game)
	| otherwise = snd (numberOfMoves game) 

-- | Returns the Board of the current playing player 
playerToMoveBoard :: Game -> Board
playerToMoveBoard game
	| (isPlaying game) == Player0 = fst (gameBoard game)
	| otherwise = snd (gameBoard game)

-- | Returns the board of the player who is not currently playing
playerToNotMoveBoard :: Game -> Board
playerToNotMoveBoard game
	| (isPlaying game) /= Player0 = fst (gameBoard game)
	| otherwise = snd (gameBoard game)  


-- | Returns the config of the player who is not currently playing
playerToNotMoveConfig :: Game -> ConfigPlayer
playerToNotMoveConfig game
	| (isPlaying game) /= Player0 = fst (configPlayer game)
	| otherwise = snd (configPlayer game)  

-- | Moves the player dice according to the key stroke provided by the user
movePlayer :: Game -> Key -> Game
movePlayer game key = 
    case key of
      (SpecialKey KeyUp) -> checkAndMove 0 1 game 
      (SpecialKey KeyDown) -> checkAndMove 0 (-1) game 
      (SpecialKey KeyRight) -> checkAndMove 1 0 game 
      (SpecialKey KeyLeft) -> checkAndMove (-1) 0 game 
      otherwise -> game

-- | Checks whether the some player is in final configuration or not
checkVictory :: Game -> Game
checkVictory game 
	| playerCoords == targetCoords && topI == targetI  = game { gameState = GameOver }
	| otherwise = game
	where
		playerCoords = findCoordsPlayer $ playerToNotMoveBoard game
		targetCoords = fst $ finalTarget game
		topI = top (playerToNotMoveConfig game)
		targetI = snd ( finalTarget game )


-- | Changes the level of the game
changeGame :: Int -> Game
changeGame i = initGameForMapLevel i 

-- | On pressing space key level is changed
startNewGame :: Game -> Key -> Game
startNewGame game key 
	| key == (SpecialKey KeySpace) = changeGame $ (level game) + 1
	| otherwise = game

-- | Transform the game based on the game state
transformGame (EventKey key Up _ _) game =
    case gameState game of
      Running -> checkVictory $ movePlayer game key
      GameOver -> startNewGame game key
transformGame _ game = game
