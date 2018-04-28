module Helper where

import Maps
import Data.Array

data ConfigPlayer = ConfigPlayer { 
		top		:: Int,
		bottom 	:: Int,
		north 	:: Int,
		south 	:: Int,
		east 	:: Int,
		west 	:: Int
	} deriving (Eq, Show)
data Cell = Wall | Player | Target  | OnTarget | Empty deriving (Eq, Show)
data GameState = Running | GameOver deriving (Eq, Show)

type Board = Array (Int, Int) Cell



data Game = Game { 
		   gameBoard 		:: Board
                 ,configPlayer 	:: ConfigPlayer
                 ,gameState 		:: GameState
                 ,finalTarget 	:: ((Int,Int),Int)
                 ,numberOfMoves 	:: Int
                 ,level	 		:: Int
                 } deriving (Eq, Show)


findAnything :: [Char] -> Int -> Char -> [Int]
findAnything [] _ _= []
findAnything (x:xs) i c 
 | x == c = i : findAnything  xs (i+1) c
 | otherwise = findAnything xs (i+1) c

findAllAnything :: [String] -> Int -> Char-> [(Int,Int)]
findAllAnything [] _ _ = []
findAllAnything (x:xs) i c = row ++ (findAllAnything xs (i-1) c) 
 	where
 		row = [ (b,a) | b<- findAnything x 0 c, let a = i ]

-- make a tuple of ((Int,Int),wall) 
tupleWall :: Int -> [((Int, Int),Cell)]
tupleWall i= zip (findAllAnything map0 (n-1) 'w') $ repeat Wall
		where
			map0 = (lMap $ maps !! i)
			n = (nI $ maps !! i)

tupleEmpty :: Int -> [((Int, Int),Cell)]
tupleEmpty i= zip (findAllAnything map0 (n-1) 'e') $ repeat Empty
		where
			map0 = (lMap $ maps !! i)
			n = (nI $ maps !! i)
tupleTarget :: Int -> [((Int, Int),Cell)]
tupleTarget i= zip (findAllAnything map0 (n-1) 't') $ repeat Target
		where
			map0 = (lMap $ maps !! i)
			n = (nI $ maps !! i)
tupleStart :: Int -> [((Int, Int),Cell)]
tupleStart i= zip (findAllAnything map0 (n-1) 's') $ repeat Player
		where
			map0 = (lMap $ maps !! i)
			n = (nI $ maps !! i)
tupleOnTarget :: Int -> [((Int, Int),Cell)]
tupleOnTarget i= zip (findAllAnything map0 (n-1) 'o') $ repeat OnTarget
		where
			map0 = (lMap $ maps !! i)
			n = (nI $ maps !! i)

initBoard :: Int -> Board
initBoard i= array indexRange $ zip (range indexRange) (repeat Empty)
	where 
		indexRange = ((0, 0), (n - 1, n - 1))
		n = (nI $ maps !! i)
-- for each tuple ((Int,Int),Cell) put it in a Board
readyBoard :: Int -> Board
readyBoard i = (initBoard i) // (tupleWall i) // (tupleEmpty i) // (tupleStart i )// (tupleTarget i)

initGameForMapLevel :: Int -> Game
initGameForMapLevel i = Game { gameBoard = readyBoard i
                   , configPlayer =  ConfigPlayer  (conFig !! 0)  (conFig !! 1) (conFig !! 2) (conFig !! 3) (conFig !! 4) (conFig !! 5 ) 
                   , gameState = Running
                   , finalTarget = final $ maps !! i
                   , numberOfMoves =0
                   , level = i
                   } 
                   where
                   	conFig = config $ maps !! i

