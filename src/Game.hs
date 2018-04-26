module Game where

import Data.Array
sizeX :: Int
sizeX = floor (gridSize * nF+ lineWidth)
sizeY :: Int
sizeY = floor (gridSize * nF + lineWidth) 
gridSize = 100.0
lineWidth = 5.0

nI :: Int
nI = 4
nF :: Float
nF = 4.0

tileSize :: Float
tileSize = 100.0


map0 :: [String]
map0= [
	 "eeee",
                        "seee",
                        "eete",
                        "eeee"
	]

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
tupleWall :: [((Int, Int),Cell)]
tupleWall = zip (findAllAnything map0 (nI-1) 'w') $ repeat Wall
tupleEmpty :: [((Int, Int),Cell)]
tupleEmpty = zip (findAllAnything map0 (nI-1) 'e') $ repeat Empty
tupleTarget :: [((Int, Int),Cell)]
tupleTarget = zip (findAllAnything map0 (nI-1) 't') $ repeat Target
tupleStart :: [((Int, Int),Cell)]
tupleStart = zip (findAllAnything map0 (nI-1) 's') $ repeat Player
tupleOnTarget :: [((Int, Int),Cell)]
tupleOnTarget = zip (findAllAnything map0 (nI-1) 'o') $ repeat OnTarget

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
                 ,finalTarget 	:: Int
                 } deriving (Eq, Show)

initBoard :: Board
initBoard = array indexRange $ zip (range indexRange) (repeat Empty)
	where indexRange = ((0, 0), (nI - 1, nI - 1))
-- for each tuple ((Int,Int),Cell) put it in a Board
readyBoard :: Board
readyBoard = initBoard // tupleWall // tupleEmpty // tupleStart // tupleTarget

initialGame = Game { gameBoard = readyBoard
                   , configPlayer = ConfigPlayer 6 1 3 4 5 2 
                   , gameState = Running
                   , finalTarget = 3
                   }
    

