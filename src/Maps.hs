module Maps (
  Level(..),
  maps
) where

-- | Provides the different levels in the game
data Level = Level { lMap            :: ([String],[String]) -- ^ Contain string of character denoting the Map of the Game. 
                    , lStartPos      :: ((Int,Int),(Int,Int)) -- ^ contains the start position of bot the players
                    , config         ::  ([Int],[Int])     -- ^ initial config of both  the players
                    , final          :: ((Int,Int),Int)    -- ^ final position with the number on the top
                    , nI             :: Int                -- ^ sqare of size n
                   } deriving Show

maps = [
		Level { lMap = (["see",
                        "eee",
                        "eet"],["eee",
                        "see",
                        "eet"])
              , lStartPos = ((0,2),(0,1))
        , config = ([6,1,3,4,5,2],[6,1,3,4,5,2])
        , final = ((2,0),1)
        , nI = 3
              },
		Level { lMap = (["seee",
                        "ewee",
                        "eete",
                        "eeee"],["esee",
                        "ewee",
                        "eete",
                        "eeee"])
              , lStartPos = ((0,3),(1,3))
        , config = ([6,1,3,4,5,2],[6,1,3,4,5,2])
        , final = ((2,1),3)
        , nI = 4
              },

        Level { lMap = (["seee",
                        "ewwe",
                        "eete",
                        "eeee"],["eeee",
                        "ewws",
                        "eete",
                        "eeee"])
              , lStartPos = ((0,3),(3,2))
        , config = ([6,1,3,4,6,2],[6,1,3,4,5,2])
        , final = ((2,1),3)
        , nI = 4
              },
		Level { lMap = (["eeeee",
                        "seeee",
                        "eeeee",
                        "eeeee",
                        "eeeet"],["eeeee",
                        "eeeee",
                        "seeee",
                        "eeeee",
                        "eeeet"])
              , lStartPos = ((0,3),(0,2))
        , config = ([6,1,4,3,2,5],[6,1,4,3,2,5])
        , final = ((4,0),5)
        , nI = 5
              },
         Level { lMap = (["eewee",
                        "seeee",
                        "eewee",
                        "eeeee",
                        "eeeet"],["eewee",
                        "eeeee",
                        "sewee",
                        "eeeee",
                        "eeeet"])
              , lStartPos = ((0,3),(0,2))
        , config = ([6,1,4,3,2,5],[6,1,4,3,2,5])
        , final = ((4,0),5)
        , nI = 5
              },
        Level { lMap = (["eeeew",
                        "sewwe",
                        "eeeee",
                        "eeeee",
                        "weeet"],["eeeew",
                        "eewwe",
                        "seeee",
                        "eeeee",
                        "weeet"])
              , lStartPos = ((0,3),(0,2))
        , config = ([6,1,4,3,2,5],[6,1,4,3,2,5])
        , final = ((4,0),5)
        , nI = 5
              }

  ]
