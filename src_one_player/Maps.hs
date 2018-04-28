module Maps (
  Level(..),
  getMap,
  maps
) where


data Level = Level { lMap            :: [String]
                                , lStartPos   :: (Int,Int)
                                , config        :: [Int]
                                , final          :: ((Int,Int),Int)
                                , nI                :: Int
                   } deriving Show

maps = [
		Level { lMap = ["see",
                        "eee",
                        "eet"]
              , lStartPos = (0,2)
        , config = [6,1,3,4,5,2]
        , final = ((2,0),1)
        , nI = 3
              },
		Level { lMap = ["seee",
                        "ewee",
                        "eete",
                        "eeee"]
              , lStartPos = (0,3)
        , config = [6,1,3,4,5,2]
        , final = ((2,1),3)
        , nI = 4
              },

        Level { lMap = ["seee",
                        "ewwe",
                        "eete",
                        "eeee"]
              , lStartPos = (0,3)
        , config = [6,1,3,4,6,2]
        , final = ((2,1),3)
        , nI = 4
              },
		Level { lMap = ["eeeee",
                        "seeee",
                        "eeeee",
                        "eeeee",
                        "eeeet"]
              , lStartPos = (0,3)
        , config = [6,1,4,3,2,5]
        , final = ((4,0),5)
        , nI = 5
              },
         Level { lMap = ["eewee",
                        "seeee",
                        "eewee",
                        "eeeee",
                        "eeeet"]
              , lStartPos = (0,3)
        , config = [6,1,4,3,2,5]
        , final = ((4,0),5)
        , nI = 5
              },
        Level { lMap = ["eeeew",
                        "sewwe",
                        "eeeee",
                        "eeeee",
                        "weeet"]
              , lStartPos = (0,3)
        , config = [6,1,4,3,2,5]
        , final = ((4,0),5)
        , nI = 5
              }

  ]



getMap level x y =
  if 0 <= x && x <= 4 &&            
     0 <= y && y <= 4
    then (lMap level !! y) !! x  
    else ' '
