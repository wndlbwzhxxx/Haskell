module Main where

import MyEvolModule
import Data.List

main :: IO ()
main = do
    putStrLn "Отсортированный список MyEvolution:"
    putStrLn $ show $ sort 
        [ LUCA
        , Cyanobacteria
        , Trilobite
        , Ichthyostega
        , Dimetrodon
        , Archaeopteryx
        , Morganucodon
        , Purgatorius
        , Australopithecine
        , Humans
        ]
    
    putStrLn "\nОтсортированный список MyEvolution' (с deriving):"
    putStrLn $ show $ sort 
        [ LUCA'
        , Cyanobacteria'
        , Trilobite'
        , Ichthyostega'
        , Dimetrodon'
        , Archaeopteryx'
        , Morganucodon'
        , Purgatorius'
        , Australopithecine'
        , Humans'
        ]
    