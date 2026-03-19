module Main (main) where

import MyTypes.MyTree ()
import MyTypes.MyMaybe ()
import MyTypes.MyEither ()

main :: IO ()
main = putStrLn "Модули MyTree, MyMaybe и MyEither успешно импортированы"