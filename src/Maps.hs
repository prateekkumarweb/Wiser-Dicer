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
        Level { lMap = (["wwee",
                        "seee",
                        "eete",
                        "eeee"],["wwee",
                        "seee",
                        "eete",
                        "eeee"])
              , lStartPos = ((0,2),(0,2))
        , config = ([6,1,3,4,5,2],[6,1,3,4,5,2])
        , final = ((2,1),3)
        , nI = 4
              },
       Level { lMap = (["eewww",
                        "seeee",
                        "eewee",
                        "eewte"],[
                        "seeee",
                        "eewee",
                        "eewee",
                        "eeete"])
              , lStartPos = ((0,3),(0,3))
        , config = ([6,1,4,3,2,5],[6,1,4,3,2,5])
        , final = ((3,0),5)
        , nI = 5
              },
       Level { lMap = (["wwww",
                        "wsee",
                        "wtee",
                        "eeee"],["wwww",
                        "wsee",
                        "wtee",
                        "eeee"])
              , lStartPos = ((0,3),(0,3))
        , config = ([6,1,3,4,5,2],[6,1,3,4,5,2])
        , final = ((0,3),3),
        nI = 4
              }

  ]
