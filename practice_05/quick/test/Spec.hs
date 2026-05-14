module Main where

import Test.QuickCheck
import Lib

genWord :: Gen String
genWord = listOf1 (elements ['a'..'z'])

genSentence :: Gen String
genSentence = do
    wordsList <- listOf1 genWord
    return $ unwords wordsList

prop_addMod_correct :: Int -> Int -> Positive Int -> Bool
prop_addMod_correct x y (Positive m) = 
    addMod x y m == (x + y) `mod` m

prop_addMod_zero :: Int -> Positive Int -> Bool
prop_addMod_zero x (Positive m) = 
    addMod x 0 m == x `mod` m

prop_addMod_commutative :: Int -> Int -> Positive Int -> Bool
prop_addMod_commutative x y (Positive m) = 
    addMod x y m == addMod y x m

prop_rev_empty :: Bool
prop_rev_empty = reverseWords "" == ""

prop_rev_singleWord :: Property
prop_rev_singleWord = forAll genWord $ \word ->
    reverseWords word == word

prop_rev_multiple :: Property
prop_rev_multiple = forAll genSentence $ \sentence ->
    let wordsList = words sentence
    in reverseWords sentence == unwords (reverse wordsList)

prop_rev_involution :: Property
prop_rev_involution = forAll genSentence $ \sentence ->
    reverseWords (reverseWords sentence) == sentence

main :: IO ()
main = do
    putStrLn "тестирование addMod"
    quickCheck prop_addMod_correct
    quickCheck prop_addMod_zero
    quickCheck prop_addMod_commutative
    
    putStrLn "\nтестирование reverseWords"
    quickCheck prop_rev_empty
    quickCheck prop_rev_singleWord
    quickCheck prop_rev_multiple
    quickCheck prop_rev_involution
    
    putStrLn "\nвсе тесты пройдены"