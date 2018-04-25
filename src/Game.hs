module Game where

import Data.Array

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


map0 :: [String]
map0= [
	 "wwwww",
	 "eeeew",
	 "wwwww",
	 "weeee",
	 "wwwww"
	]

findWall :: [Char] -> Int -> [Int]
findWall [] _ = []
findWall (x:xs) i 
 | x == 'w' = i : findWall  xs (i+1)
 | otherwise = findWall xs (i+1)

findAllWall :: [String] -> Int -> [(Int,Int)]
findAllWall [] _ = []
findAllWall  (x:xs) i = row ++ (findAllWall xs (i-1)) 
 	where
 		row = [ (b,a) | b<- findWall x 0, let a = i ]

-- make a tuple of ((Int,Int),wall) 
tupleWall :: [((Int, Int),Cell)]
tupleWall = zip (findAllWall map0 (nI-1)) $ repeat Wall

data ConfigPlayer = ConfigPlayer { 
		top		:: Int,
		bottom 	:: Int,
		north 	:: Int,
		south 	:: Int,
		east 	:: Int,
		west 	:: Int
	} deriving (Eq, Show)
data Cell = Wall | Player | Target (Int) | OnTarget | Empty deriving (Eq, Show)
data GameState = Running | GameOver deriving (Eq, Show)
type Board = Array (Int, Int) Cell

data Game = Game { 
		   gameBoard :: Board
                 ,configPlayer :: ConfigPlayer
                 ,gameState :: GameState
                 } deriving (Eq, Show)

initBoard :: Board
initBoard = array indexRange $ zip (range indexRange) (repeat Empty)
	where indexRange = ((0, 0), (nI - 1, nI - 1))
-- for each tuple ((Int,Int),Cell) put it in a Board
readyBoard :: Board
readyBoard = initBoard // tupleWall

initialGame = Game { gameBoard = readyBoard
                   , configPlayer = ConfigPlayer 6 1 3 4 5 2 
                   , gameState = Running
                   }
    

