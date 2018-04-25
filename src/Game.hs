module Game where

import Data.Array
data Player = Player { 
                top             :: Int,
                bottom  :: Int,
                north   :: Int,
                south   :: Int,
                east    :: Int,
                west    :: Int
        } deriving (Eq, Show)
data Cell = wall | player | target (Int) | onTarget | Nothing deriving (Eq, Show)
data gameState = Running | GameOver deriving (Eq, Show)
type Board = Array (Int, Int) Cell

data Game = Game { 
                   gameBoard :: Board
                 ,configPlayer :: Player
                 ,gameState :: State
                 } deriving (Eq, Show)

initialGame = Game { gameBoard = array indexRange $ zip (range indexRange) (repeat Nothing)
                   , configPlayer 6 1 3 4 5 2 
                   , gameState = Running
                   }
    where indexRange = ((0, 0), (n - 1, n - 1))
