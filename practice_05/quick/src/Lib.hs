module Lib
    ( addMod
    , reverseWords
    ) where

addMod :: Int -> Int -> Int -> Int
addMod x y m = (x + y) `mod` m

reverseWords :: String -> String
reverseWords = unwords . reverse . words