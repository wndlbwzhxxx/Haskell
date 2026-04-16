{-# LANGUAGE OverloadedStrings #-}

module Glitch
  ( replaceByte
  , sortSection
  , reverseSection
  , randomReplaceByte
  , randomSortSection
  , randomReverseBytes
  , applyRandomReplacements
  , applyRandomSorts
  , applyRandomReverses
  , applyFullGlitch
  , applyFullGlitchWithReverse
  ) where

import qualified Data.ByteString.Char8 as BC
import System.Random (randomRIO)
import Control.Monad (foldM)

-- заменяет байт в заданной позиции
replaceByte :: Int -> Int -> BC.ByteString -> BC.ByteString
replaceByte pos newVal bytes = mconcat [before, newChar, after]
  where
    (before, rest) = BC.splitAt pos bytes
    after = BC.drop 1 rest
    newChar = BC.pack [toEnum (newVal `mod` 255)]

-- сортирует участок байтов
sortSection :: Int -> Int -> BC.ByteString -> BC.ByteString
sortSection start size bytes = mconcat [before, changed, after]
  where
    (before, rest) = BC.splitAt start bytes
    (target, after) = BC.splitAt size rest
    changed = BC.sort target

-- переворачивает участок байтов
reverseSection :: Int -> Int -> BC.ByteString -> BC.ByteString
reverseSection start size bytes = mconcat [before, changed, after]
  where
    (before, rest) = BC.splitAt start bytes
    (target, after) = BC.splitAt size rest
    changed = BC.reverse target

-- случайная замена одного байта
randomReplaceByte :: BC.ByteString -> IO BC.ByteString
randomReplaceByte bytes = do
  let len = BC.length bytes
  if len <= 1
    then return bytes
    else do
      pos <- randomRIO (1, len - 1)
      newVal <- randomRIO (0, 255)
      return (replaceByte pos newVal bytes)

-- случайная сортировка участка
randomSortSection :: BC.ByteString -> IO BC.ByteString
randomSortSection bytes = do
  let len = BC.length bytes
      sectionSize = 25
  if len <= sectionSize
    then return bytes
    else do
      start <- randomRIO (0, len - sectionSize)
      return (sortSection start sectionSize bytes)

-- случайный переворот участка
randomReverseBytes :: BC.ByteString -> IO BC.ByteString
randomReverseBytes bytes = do
  let len = BC.length bytes
      sectionSize = 25
  if len <= sectionSize
    then return bytes
    else do
      start <- randomRIO (0, len - sectionSize)
      return (reverseSection start sectionSize bytes)

-- применяет N случайных замен байтов
applyRandomReplacements :: BC.ByteString -> Int -> IO BC.ByteString
applyRandomReplacements bytes 0 = return bytes
applyRandomReplacements bytes n = do
  newBytes <- randomReplaceByte bytes
  applyRandomReplacements newBytes (n - 1)

-- применяет N случайных сортировок участков
applyRandomSorts :: BC.ByteString -> Int -> IO BC.ByteString
applyRandomSorts bytes 0 = return bytes
applyRandomSorts bytes n = do
  newBytes <- randomSortSection bytes
  applyRandomSorts newBytes (n - 1)

-- применяет N случайных переворотов участков
applyRandomReverses :: BC.ByteString -> Int -> IO BC.ByteString
applyRandomReverses bytes 0 = return bytes
applyRandomReverses bytes n = do
  newBytes <- randomReverseBytes bytes
  applyRandomReverses newBytes (n - 1)

-- полный глитч: замена -> сортировка -> замена -> сортировка -> замена
applyFullGlitch :: BC.ByteString -> IO BC.ByteString
applyFullGlitch bytes = foldM (\b f -> f b) bytes
  [ randomReplaceByte
  , randomSortSection
  , randomReplaceByte
  , randomSortSection
  , randomReplaceByte
  ]

-- полный глитч с переворотом вместо сортировки (задача 25.2)
applyFullGlitchWithReverse :: BC.ByteString -> IO BC.ByteString
applyFullGlitchWithReverse bytes = foldM (\b f -> f b) bytes
  [ randomReplaceByte
  , randomReverseBytes
  , randomReplaceByte
  , randomReverseBytes
  , randomReplaceByte
  ]