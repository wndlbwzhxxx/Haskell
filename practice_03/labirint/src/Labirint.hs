{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BangPatterns #-}

module Labirint (
    Room(..)
    , Maze
    , MazeState(..)
    , MazeLog(..)
    , MazeRWS
    , findPath
    , solveMaze
    , labirint1
) where

import Control.Monad.RWS (RWS, runRWS, ask, tell, get, put, modify)


data Room = Room {
    roomName :: String
    , roomDescription :: String
    , connections :: [(String, String)]
} deriving (Show, Eq)

type Maze = [(String, Room)]

data MazeState = MazeState {
    currentRoom :: String
    , visitedRooms :: [String]
    , path :: [String]
    , steps :: Int
} deriving (Show)

newtype MazeLog = MazeLog { getLog :: [String] }
    deriving (Show)

instance Semigroup MazeLog where
    (MazeLog l1) <> (MazeLog l2) = MazeLog (l1 ++ l2)

instance Monoid MazeLog where
    mempty = MazeLog []
    mappend = (<>)

type MazeRWS a = RWS Maze MazeLog MazeState a


getCurrentRoom :: MazeRWS String
getCurrentRoom = fmap currentRoom get

getVisitedRooms :: MazeRWS [String]
getVisitedRooms = fmap visitedRooms get

isVisited :: String -> MazeRWS Bool
isVisited room = 
    getVisitedRooms >>= \visited ->
    return (room `elem` visited)

markVisited :: String -> MazeRWS ()
markVisited room = 
    get >>= \s ->
    put (s { visitedRooms = room : visitedRooms s
           , path = path s ++ [room]
           , currentRoom = room
           , steps = steps s + 1 })

logMovement :: String -> String -> String -> MazeRWS ()
logMovement from to direction = 
    get >>= \s ->
    let msg = "Step " ++ show (steps s) ++ ": " ++ direction ++ " from '" ++ from ++ "' to '" ++ to ++ "'"
    in tell (MazeLog [msg])

getRoom :: String -> MazeRWS Room
getRoom name = 
    ask >>= \maze ->
    case lookup name maze of
        Just room -> return room
        Nothing -> error $ "Room not found: " ++ name

getNeighbors :: String -> MazeRWS [(String, String)]
getNeighbors roomName = 
    getRoom roomName >>= \room ->
    return (connections room)

moveToRoom :: String -> String -> MazeRWS Bool
moveToRoom from to = 
    getNeighbors from >>= \neighbors ->
    let canMove = any (\(_, room) -> room == to) neighbors
    in if canMove
        then let swap (a,b) = (b,a)
                 dir = lookup to (map swap neighbors)
             in case dir of
                 Just d -> logMovement from to d
                 Nothing -> return ()
                >> markVisited to
                >> return True
        else return False


findPath :: String -> String -> MazeRWS Bool
findPath start finish =
    markVisited start >>
    findPathFromCurrent finish

findPathFromCurrent :: String -> MazeRWS Bool
findPathFromCurrent finish =
    getCurrentRoom >>= \cur ->
    if cur == finish
        then tell (MazeLog ["FINISH reached: " ++ finish]) >> return True
        else getVisitedRooms >>= \visited ->
             getNeighbors cur >>= \neighbors ->
             let unvisited = filter (\(_, r) -> r `notElem` visited) neighbors
             in tryPaths cur unvisited finish

tryPaths :: String -> [(String, String)] -> String -> MazeRWS Bool
tryPaths from [] finish =
    tell (MazeLog ["Dead end from '" ++ from ++ "', backtracking..."]) >>
    return False
tryPaths from ((dir, nextRoom):rest) finish =
    moveToRoom from nextRoom >>= \success ->
    if success
        then findPathFromCurrent finish >>= \found ->
             if found
                 then return True
                 else get >>= \s ->
                      tell (MazeLog ["Backtrack from '" ++ nextRoom ++ "', trying next path from '" ++ from ++ "'"]) >>
                      tryPaths from rest finish
        else tryPaths from rest finish


solveMaze :: Maze -> String -> String -> IO ()
solveMaze maze start finish = do
    let initialState = MazeState {
            currentRoom = start
            , visitedRooms = []
            , path = []
            , steps = 0
        }
    let triple = runRWS (findPath start finish) maze initialState
    let found = case triple of (a, _, _) -> a
    let finalState = case triple of (_, s, _) -> s
    let log = case triple of (_, _, w) -> w
    
    putStrLn ""
    putStrLn "========================================"
    putStrLn "LABYRINTH PATH FINDER"
    putStrLn "========================================"
    putStrLn ("Start: " ++ start)
    putStrLn ("Finish: " ++ finish)
    putStrLn ("Path found: " ++ if found then "YES" else "NO")
    putStrLn ("Steps: " ++ show (steps finalState))
    putStrLn ""
    putStrLn "Path taken:"
    putStrLn ("  " ++ show (path finalState))
    putStrLn ""
    putStrLn "Movement log:"
    mapM_ (\x -> putStrLn ("  " ++ x)) (getLog log)
    putStrLn "========================================"
    return ()

labirint1 :: Maze
labirint1 = 
    [ ("start", Room "start" "Mondstadt City Gate" [("east", "dragonspine")])
    
    , ("dragonspine", Room "dragonspine" "Frozen Mountain Peak" [("west", "start"), ("south", "liyue_harbor")])
    
    , ("liyue_harbor", Room "liyue_harbor" "Busy Harbor Port" [("south", "qingce_village"), ("east", "jueyun_karst"), ("north", "dragonspine")])

    , ("qingce_village", Room "qingce_village" "Bamboo Forest Village" [("north", "liyue_harbor"), ("south", "chasm")])
    
    , ("jueyun_karst", Room "jueyun_karst" "Mysterious Mountain Range" [("west", "liyue_harbor"), ("south", "inazuma_city")])
    
    , ("chasm", Room "chasm" "Deep Underground Mines" [("north", "qingce_village"), ("east", "inazuma_city")])
    
    , ("inazuma_city", Room "inazuma_city" "Electro Archon's Capital" [("south", "narukami_shrine"), ("north", "jueyun_karst"), ("west", "chasm")])

    , ("narukami_shrine", Room "narukami_shrine" "Sacred Shrine on the Mountain" [("west", "abandoned_ruins"), ("south", "finish"), ("north", "inazuma_city")])
    
    , ("abandoned_ruins", Room "abandoned_ruins" "Abandoned Ruins of Sal Vindagnyr" [("east", "narukami_shrine")])

    , ("finish", Room "finish" "Exit from Labyrinth" [("north", "narukami_shrine")])
    ]