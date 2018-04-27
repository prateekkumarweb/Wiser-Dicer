module Maps (
  Level(..),
  maps
) where


data Level = Level { lMap            :: ([String],[String])
                                , lStartPos   :: ((Int,Int),(Int,Int))
                                , config        ::  ([Int],[Int])
                                , final          :: ((Int,Int),Int)
                                , nI                :: Int
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
        , nI = 4
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


-- getMap level x y =
--   if 0 <= x && x <= 4 &&            
--      0 <= y && y <= 4
--     then (lMap level !! y) !! x  
--     else ' '
