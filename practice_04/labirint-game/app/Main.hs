module Main where

import MyStateT
import MazeTypes
import MazeLoader
import GameMonad
import qualified Data.Map as Map
import System.IO
import Control.Monad.IO.Class (liftIO)
import System.Environment (getArgs)

showHelp :: IO ()
showHelp = do
    putStrLn "Commands: look, north, south, west, east, quit"

gameLoop :: String -> GameMonad Bool
gameLoop finishRoom = do
    st <- get
    let current = currentRoomName st
    if current == finishRoom
        then do
            logMessage "You reached the finish!"
            return True
        else do
            room <- getCurrentRoomInfo
            liftIO $ putStrLn ""
            liftIO $ putStrLn $ "Location: " ++ roomName room
            liftIO $ putStrLn $ roomDescription room
            liftIO $ putStr "> "
            liftIO $ hFlush stdout
            command <- liftIO getLine
            case command of
                "quit" -> do
                    logMessage "Game ended"
                    return True
                "help" -> do
                    liftIO showHelp
                    gameLoop finishRoom
                "look" -> do
                    let dirs = neighbors room
                    if null dirs
                        then liftIO $ putStrLn "No exits"
                        else do
                            let exitsList = map (\(d, r) -> d ++ " (" ++ r ++ ")") dirs
                            liftIO $ putStrLn $ "Exits: " ++ unwords exitsList
                    gameLoop finishRoom
                "north" -> do
                    result <- processCommand command
                    if not result
                        then liftIO $ putStrLn "Wrong! Cannot go north from here"
                        else return ()
                    gameLoop finishRoom
                "south" -> do
                    result <- processCommand command
                    if not result
                        then liftIO $ putStrLn "Wrong! Cannot go south from here"
                        else return ()
                    gameLoop finishRoom
                "west" -> do
                    result <- processCommand command
                    if not result
                        then liftIO $ putStrLn "Wrong! Cannot go west from here"
                        else return ()
                    gameLoop finishRoom
                "east" -> do
                    result <- processCommand command
                    if not result
                        then liftIO $ putStrLn "Wrong! Cannot go east from here"
                        else return ()
                    gameLoop finishRoom
                _ -> do
                    liftIO $ putStrLn $ "Wrong! Unknown command: " ++ command
                    gameLoop finishRoom

startGame :: Maze -> String -> String -> IO ()
startGame maze startRoom finishRoom = do
    putStrLn "=== MAZE GAME ==="
    putStrLn $ "Start: " ++ startRoom
    putStrLn $ "Goal: " ++ finishRoom
    putStrLn "Commands: look, north, south, west, east, quit, help"
    
    let initialState = initGameState startRoom
    (result, finalState, logs) <- runGame (gameLoop finishRoom) maze initialState
    
    putStrLn ""
    putStrLn "=== GAME OVER ==="
    if currentRoomName finalState == finishRoom
        then putStrLn "Victory! You found the exit."
        else putStrLn "Game terminated."
    putStrLn $ "Total moves: " ++ show (movesCount finalState)
    putStrLn $ "Final room: " ++ currentRoomName finalState
    putStrLn ""
    putStrLn "Movement log:"
    mapM_ putStrLn logs

main :: IO ()
main = do
    args <- getArgs
    let mazeFile = if null args then "maze.txt" else head args
    
    putStrLn $ "Loading maze from: " ++ mazeFile
    maze <- loadMazeFromFile mazeFile
    
    let startRoom = "start"
        finishRoom = "finish"
    
    if not (roomExists maze startRoom)
        then error $ "Start room '" ++ startRoom ++ "' not found in " ++ mazeFile
        else return ()
    
    if not (roomExists maze finishRoom)
        then error $ "Finish room '" ++ finishRoom ++ "' not found in " ++ mazeFile
        else return ()
    
    startGame maze startRoom finishRoom