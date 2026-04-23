module Main where

import MazeTypes
import MazeLoader
import qualified Data.Map as Map

main :: IO ()
main = do
    maze <- loadMazeFromFile "maze.txt"
    putStrLn $ "Loaded " ++ show (Map.size maze) ++ " rooms"
    putStrLn $ "Rooms: " ++ unwords (Map.keys maze)
    let start = "start"
    case Map.lookup start maze of
        Just room -> do
            putStrLn $ "Start room: " ++ roomName room
            putStrLn $ "Description: " ++ roomDescription room
            putStrLn $ "Exits: " ++ show (map fst (neighbors room))
        Nothing -> putStrLn "Start room not found"