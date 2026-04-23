{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# LANGUAGE FlexibleContexts #-}

module GameMonad (
    GameState(..),
    GameLog,
    GameMonad,
    runGame,
    getCurrentRoomInfo,
    logMessage,
    moveDirection,
    getAvailableDirections,
    isFinish,
    processCommand,
    initGameState
) where

import MyStateT
import MazeTypes
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Writer (WriterT, tell, runWriterT)
import Control.Monad.Trans.Class (lift)
import qualified Data.Map as Map

data GameState = GameState {
    currentRoomName :: String
    , visitedRooms :: [String]
    , movesCount :: Int
} deriving (Show)

type GameLog = [String]

type GameMonad a = MyStateT GameState (WriterT GameLog (ReaderT Maze IO)) a

runGame :: GameMonad a -> Maze -> GameState -> IO (a, GameState, GameLog)
runGame game maze st = do
    ((a, state), log) <- runReaderT (runWriterT (runMyStateT game st)) maze
    return (a, state, log)

getMaze :: GameMonad Maze
getMaze = lift $ lift ask

getCurrentRoomInfo :: GameMonad Room
getCurrentRoomInfo = do
    st <- get
    maze <- getMaze
    case Map.lookup (currentRoomName st) maze of
        Just room -> return room
        Nothing -> error $ "Room not found: " ++ currentRoomName st

logMessage :: String -> GameMonad ()
logMessage msg = lift $ tell [msg]

moveDirection :: String -> GameMonad Bool
moveDirection dir = do
    st <- get
    let current = currentRoomName st
    currentRoom <- getCurrentRoomInfo
    let targetRoom = case dir of
            "north" -> north currentRoom
            "south" -> south currentRoom
            "west" -> west currentRoom
            "east" -> east currentRoom
            _ -> Nothing
    case targetRoom of
        Just roomName -> do
            let newState = st {
                    currentRoomName = roomName,
                    visitedRooms = roomName : visitedRooms st,
                    movesCount = movesCount st + 1
                }
            put newState
            logMessage $ dir ++ " from " ++ current ++ " to " ++ roomName
            return True
        Nothing -> do
            logMessage $ "Cannot go " ++ dir ++ " from " ++ current
            return False

getAvailableDirections :: GameMonad [(String, String)]
getAvailableDirections = do
    room <- getCurrentRoomInfo
    return $ neighbors room

isFinish :: String -> GameMonad Bool
isFinish finishRoom = do
    st <- get
    return $ currentRoomName st == finishRoom

processCommand :: String -> GameMonad Bool
processCommand cmd = do
    case cmd of
        "north" -> moveDirection "north"
        "south" -> moveDirection "south"
        "west" -> moveDirection "west"
        "east" -> moveDirection "east"
        "quit" -> return True
        _ -> return False

initGameState :: String -> GameState
initGameState startRoom = GameState startRoom [startRoom] 0