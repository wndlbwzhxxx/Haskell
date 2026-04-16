module Main where

import Labirint

main :: IO ()
main = 
    putStrLn "\n=== Genshin Impact Labyrinth Path Finder (RWS Monad) ==="
    >> solveMaze labirint1 "start" "finish"
    >> putStrLn "\n=== Program finished ==="