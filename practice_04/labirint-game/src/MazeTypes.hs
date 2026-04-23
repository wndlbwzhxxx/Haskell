module MazeTypes where

import Data.Map (Map)
import qualified Data.Map as Map

data Room = Room {
    roomName :: String
    , roomDescription :: String
    , north :: Maybe String
    , south :: Maybe String
    , west :: Maybe String
    , east :: Maybe String
} deriving (Show, Eq)

type Maze = Map String Room

neighbors :: Room -> [(String, String)]
neighbors room = 
    [("north", n) | Just n <- [north room]] ++
    [("south", s) | Just s <- [south room]] ++
    [("west", w) | Just w <- [west room]] ++
    [("east", e) | Just e <- [east room]]

roomExists :: Maze -> String -> Bool
roomExists maze name = Map.member name maze