module MazeLoader where

import MazeTypes
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (isPrefixOf)

parseRoomLine :: String -> Maybe (String, Room)
parseRoomLine line
    | null line = Nothing
    | "#" `isPrefixOf` line = Nothing
    | otherwise =
        case splitOn ':' line of
            (name:desc:rest) ->
                let fields = take 4 (rest ++ repeat "")
                    n = if length rest > 0 then rest !! 0 else ""
                    s = if length rest > 1 then rest !! 1 else ""
                    w = if length rest > 2 then rest !! 2 else ""
                    e = if length rest > 3 then rest !! 3 else ""
                    toMaybe "" = Nothing
                    toMaybe str = Just str
                in Just (name, Room name desc (toMaybe n) (toMaybe s) (toMaybe w) (toMaybe e))
            _ -> Nothing

splitOn :: Char -> String -> [String]
splitOn _ [] = []
splitOn c s =
    case break (== c) s of
        (part, []) -> [part]
        (part, (_:rest)) -> part : splitOn c rest

loadMazeFromFile :: FilePath -> IO Maze
loadMazeFromFile filename = do
    contents <- readFile filename
    let linesList = filter (not . null) (lines contents)
        rooms = mapMaybe parseRoomLine linesList
    return $ Map.fromList rooms

mapMaybe :: (a -> Maybe b) -> [a] -> [b]
mapMaybe _ [] = []
mapMaybe f (x:xs) =
    case f x of
        Just y -> y : mapMaybe f xs
        Nothing -> mapMaybe f xs