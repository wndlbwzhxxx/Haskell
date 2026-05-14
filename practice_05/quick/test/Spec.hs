module Main where

import Test.QuickCheck
import Lib

prop_addMod_correct :: Int -> Int -> Positive Int -> Bool
prop_addMod_correct x y (Positive m) = 
    addMod x y m == (x + y) `mod` m

prop_addMod_zero :: Int -> Positive Int -> Bool
prop_addMod_zero x (Positive m) = 
    addMod x 0 m == x `mod` m

prop_addMod_commutative :: Int -> Int -> Positive Int -> Bool
prop_addMod_commutative x y (Positive m) = 
    addMod x y m == addMod y x m

main :: IO ()
main = do
    putStrLn "тестирование addMod"
    quickCheck prop_addMod_correct
    quickCheck prop_addMod_zero
    quickCheck prop_addMod_commutative