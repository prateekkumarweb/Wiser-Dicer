module Maps (
  Level(..),
  getMap,
  maps
) where


data Level = Level { lMap      :: [String]
                   , lStartPos :: (Int,Int)
       , config   :: [Int]
       , final     :: (Int,Int)
                   } deriving Show
