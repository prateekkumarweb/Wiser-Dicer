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

maps = [
        Level { lMap = ["bbb ",
                        "bbb ",
                        "bbb ",
                        "bbb "]
              , lStartPos = (0,2)
        , config = [6,1,3,4,5,2]
        , final = (2,1)
              },
       Level { lMap = [ "b  b",
                        "b  b",
                        "bbbb",
                        "bbbb"]
              , lStartPos = (0,3)
        , config = [6,1,4,3,2,5]
        , final = (1,0)
              },
       Level { lMap = [ "bbbb",
                        "bb b",
                        "bb b",
                        "bbbb"]
              , lStartPos = (0,3)
        , config = [6,1,3,4,5,2]
        , final = (0,3)
              }

  ]


getMap level x y =
  if 0 <= x && x <= 4 &&            
     0 <= y && y <= 4
    then (lMap level !! y) !! x  
    else ' '
