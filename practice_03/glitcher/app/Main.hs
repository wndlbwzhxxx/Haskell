{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BC
import System.Environment (getArgs)
import Glitch

main :: IO ()
main = do
  args <- getArgs
  if null args
    then putStrLn "ошибка: укажите имя файла с изображением"
    else do
      let fileName = head args
      putStrLn $ "чтение файла: " ++ fileName
      imageFile <- BC.readFile fileName
      
      putStrLn "\n1: замена байтов"
      glitched1 <- applyRandomReplacements imageFile 5
      BC.writeFile ("replace_" ++ fileName) glitched1
      putStrLn $ "сохранено: replace_" ++ fileName
      
      putStrLn "\n2: сортировка участков"
      glitched2 <- applyRandomSorts imageFile 5
      BC.writeFile ("sort_" ++ fileName) glitched2
      putStrLn $ "сохранено: sort_" ++ fileName
      
      putStrLn "\n3: комбинированный (замена + сортировка)"
      glitched3 <- applyFullGlitch imageFile
      BC.writeFile ("glitched_" ++ fileName) glitched3
      putStrLn $ "сохранено: glitched_" ++ fileName
      
      putStrLn "\n4: комбинированный с переворотом"
      glitched4 <- applyFullGlitchWithReverse imageFile
      BC.writeFile ("reverse_" ++ fileName) glitched4
      putStrLn $ "сохранено: reverse_" ++ fileName
      
      putStrLn "\n5: только переворот участков"
      glitched5 <- applyRandomReverses imageFile 5
      BC.writeFile ("only_reverse_" ++ fileName) glitched5
      putStrLn $ "сохранено: only_reverse_" ++ fileName
      
      putStrLn "\nготово! созданы файлы:"
      putStrLn $ "  1. replace_" ++ fileName
      putStrLn $ "  2. sort_" ++ fileName
      putStrLn $ "  3. glitched_" ++ fileName
      putStrLn $ "  4. reverse_" ++ fileName
      putStrLn $ "  5. only_reverse_" ++ fileName