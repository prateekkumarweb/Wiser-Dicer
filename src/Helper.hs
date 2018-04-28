module Helper where

import Maps
import Data.Array


-- | Data for storing the configuration of the dice at any time
data ConfigPlayer = ConfigPlayer { 
		top		:: Int,
		bottom 	:: Int,
		north 	:: Int,
		south 	:: Int,
		east 	:: Int,
		west 	:: Int
	} deriving (Eq, Show)

-- | Data representing the types of cell
data Cell = Wall | Player | Target  | OnTarget | Empty deriving (Eq, Show)


-- | Data representing thr game state at any time in the game
data GameState = Running | GameOver deriving (Eq, Show)

-- | Player turn 
data Turn = Player0 | Player1 deriving(Eq, Show)


type Board = Array (Int, Int) Cell

-- | Contains total information of the game at any given time
data Game = Game { 
		   gameBoard 		:: (Board,Board)
                 ,configPlayer 	:: (ConfigPlayer,ConfigPlayer)
                 ,gameState 		:: GameState
                 ,finalTarget 	:: ((Int,Int),Int)
                 ,numberOfMoves 	:: (Int,Int)
                 ,level	 		:: Int
                 ,isPlaying 		:: Turn 
                 } deriving (Eq, Show)


-- | Converts the string representing map into original map
findAnything :: [Char] -> Int -> Char -> [Int]
findAnything [] _ _= []
findAnything (x:xs) i c 
 | x == c = i : findAnything  xs (i+1) c
 | otherwise = findAnything xs (i+1) c


-- | Converts the string representing map into original map
findAllAnything :: [String] -> Int -> Char-> [(Int,Int)]
findAllAnything [] _ _ = []
findAllAnything (x:xs) i c = row ++ (findAllAnything xs (i-1) c) 
 	where
 		row = [ (b,a) | b<- findAnything x 0 c, let a = i ]

-- | make a tuple of ((Int,Int),wall) 
tupleWall :: Int -> [((Int, Int),Cell)]
tupleWall i= zip (findAllAnything map0 (n-1) 'w') $ repeat Wall
		where
			map0 = fst (lMap $ maps !! i)
			n = (nI $ maps !! i)
-- | make a tuple of ((Int,Int),Empty)
tupleEmpty :: Int -> [((Int, Int),Cell)]
tupleEmpty i= zip (findAllAnything map0 (n-1) 'e') $ repeat Empty
		where
			map0 = fst (lMap $ maps !! i)
			n = (nI $ maps !! i)

-- | make a tuple of ((Int,Int),Target)
tupleTarget :: Int -> [((Int, Int),Cell)]
tupleTarget i= zip (findAllAnything map0 (n-1) 't') $ repeat Target
		where
			map0 = fst (lMap $ maps !! i)
			n = (nI $ maps !! i)


-- | make a tuple of ((Int,Int),Start)
tupleStart :: Int -> ([((Int, Int),Cell)],[((Int, Int),Cell)])
tupleStart i= ((zip (findAllAnything map00 (n-1) 's') $ repeat Player),(zip (findAllAnything map01 (n-1) 's') $ repeat Player))
		where
			map00 = fst (lMap $ maps !! i)
			map01 = snd (lMap $ maps !! i)
			n = (nI $ maps !! i)

-- | make a tuple of ((Int,Int),onTarget)
tupleOnTarget :: Int -> [((Int, Int),Cell)]
tupleOnTarget i= zip (findAllAnything map0 (n-1) 'o') $ repeat OnTarget
		where
			map0 = fst (lMap $ maps !! i)
			n = (nI $ maps !! i)


-- | make a tuple of ((Int,Int),Empty)
initBoard :: Int -> (Board)
initBoard i= array indexRange $ zip (range indexRange) (repeat Empty)
	where 
		indexRange = ((0, 0), (n - 1, n - 1))
		n = (nI $ maps !! i)


-- | for each tuple ((Int,Int),Cell) put it in a Board
readyBoard :: Int -> (Board,Board)
readyBoard i = ( (initBoard i) // (tupleWall i) // (tupleEmpty i) // fst (tupleStart i ) // (tupleTarget i),
				(initBoard i) // (tupleWall i) // (tupleEmpty i) // snd (tupleStart i ) // (tupleTarget i) )


-- | Initilalises the game for the given level
initGameForMapLevel :: Int  -- ^ 0 level0 ans so on 
					-> Game  -- ^ returns the game accordingly
initGameForMapLevel i = Game { gameBoard = readyBoard i
                   , configPlayer =  (configP1, configP2)
                   , gameState = Running
                   , finalTarget = final $ maps !! i
                   , numberOfMoves = (0,0)
                   , level = i
                   , isPlaying = Player0
                   } 
                   where
                   	conFig1 = fst $ config $ maps !! i
                   	conFig2 = snd $ config $ maps !! i
                   	configP1 = ConfigPlayer  (conFig1 !! 0)  (conFig1 !! 1) (conFig1 !! 2) (conFig1 !! 3) (conFig1 !! 4) (conFig1 !! 5 ) 
                   	configP2 = ConfigPlayer  (conFig2 !! 0)  (conFig2 !! 1) (conFig2 !! 2) (conFig2 !! 3) (conFig2 !! 4) (conFig2 !! 5 ) 
