-------------------------------------
-- Практические задание 1. Часть 1 --
-------------------------------------

module Pr01_1 where
import System.Win32 (xBUTTON1, COORD (yPos))

{-

Напишите реализацию функций myFST, mySND, myTHRD для кортежа (a,b,c)

Напишите реализацию стандартных функции для работы со списками:
-- myHead - определение (через сопоставление с образцом) функции отделения головы списка
-- myTail - функция отделения хвоста списка
-- myTake - взять первые n элементов списка
-- myDrop - отбросить первые n элементов списка
-- myProduct - перемножить все элементы списка
-- myZip - попарное объединение двух списков в список пар, длина итогового списка по длине самого короткого из входных списков
-- myZip3 объединение трех списков в список троек
-- myUnzip - разделение списка пар на пару списков

Напишите реализацию стандартных функции высшего порядка для работы со списками:
-- myFilter - применение предиката к каждому элементу списка (две реализации: с использованием охранных выражений и if-then-else)
-- myMap - применение функции одного аргумента к каждому элементу списка
-- myZipWith - применение функции двух аргументов к двум спискам
-- myZipWith3 - применение функции трех аргументов к трем спискам
-- myAll - проверяет удовлетворяют ли все элементы списка предикату
-- myAny - проверяет удовлетворяют ли хотя бы один элемент списка предикату
-- myComposition - композиция двух функций (.)

-}

-- Напишите реализацию функций myFST, mySND, myTHRD для кортежа (a,b,c)

myFST :: (a, b, c) -> a
myFST (x, y, z) = x

mySND :: (a, b, c) -> b
mySND (x, y, z) = y

myTHRD :: (a, b, c) -> c
myTHRD (x, y, z) = z


-- Напишите реализацию стандартных функции для работы со списками:

-- myHead - определение (через сопоставление с образцом) функции отделения головы списка

myHead :: [a] -> a
myHead [] = error "list is empty"
myHead (x : xs) = x

-- myTail - функция отделения хвоста списка

myTail :: [a] -> [a]
myTail [] = error "list is empty"
myTail (x : xs) = xs

-- myTake - взять первые n элементов списка

myTake :: Int -> [a] -> [a]
myTake 0 xs = []
myTake n [] = []
myTake n (x : xs) = x : myTake (n - 1) xs

-- myDrop - отбросить первые n элементов списка

myDrop :: Int -> [a] -> [a]
myDrop 0 xs = xs
myDrop n [] = []
myDrop n (x : xs) = myDrop (n - 1) xs

-- myProduct - перемножить все элементы списка

myProduct :: [Int] -> Int
myProduct [] = error "list is empty"
myProduct [xs] = xs
myProduct (x : xs) = x * myProduct xs

myProductf :: [Int] -> Int
myProductf xs = foldr (*) 1 xs 

-- myZip - попарное объединение двух списков в список пар, длина итогового списка по длине самого короткого из входных списков

myZip :: [a] -> [b] -> [(a, b)]
myZip [] ys = []
myZip xs [] = []
myZip (x : xs) (y : ys) = (x, y) : myZip xs ys

-- myZip3 объединение трех списков в список троек

myZip3 :: [a] -> [b] -> [c] -> [(a, b, c)]
myZip3 [] ys zs = []
myZip3 xs [] zs = []
myZip3 xs ys [] = []
myZip3 (x : xs) (y : ys) (z : zs) = (x, y, z) : myZip3 xs ys zs

-- myUnzip - разделение списка пар на пару списков

myUnzip0 :: [(a,b)] -> ([a],[b])
myUnzip0 [] = ([], [])
myUnzip0 ((x,y): xys) = (x : xs, y : ys) 
    where (xs, ys) = myUnzip0 xys

myUnzip1 :: [(a,b)] -> ([a],[b])
myUnzip1 [] = ([], [])
myUnzip1 ((x,y): xys) = 
    let (xs, ys) = myUnzip1 xys
    in (x : xs, y : ys)

myUnzip2 :: [(a,b)] -> ([a],[b])
myUnzip2 pairs = unzipHelper pairs [] []
    where
        unzipHelper [] acc1 acc2 = (reverse acc1, reverse acc2)
        unzipHelper ((x,y):rest) acc1 acc2 = unzipHelper rest (x:acc1) (y:acc2)


-- Напишите реализацию стандартных функции высшего порядка для работы со списками:

-- myFilter - применение предиката к каждому элементу списка (две реализации: с использованием охранных выражений и if-then-else)

myFilter0 :: (a -> Bool) -> [a] -> [a]
myFilter0 predicate [] = []
myFilter0 predicate (x : xs)
    | predicate x = x : myFilter0 predicate xs
    | otherwise   = myFilter0 predicate xs


myFilter1 :: (a -> Bool) -> [a] -> [a]
myFilter1 predicate [] = []
myFilter1 predicate (x : xs) = 
    if predicate x
        then x : myFilter1 predicate xs
        else myFilter1 predicate xs

-- myMap - применение функции одного аргумента к каждому элементу списка

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x : xs) = f x : myMap f xs

-- myZipWith - применение функции двух аргументов к двум спискам

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ _ [] = []
myZipWith _ [] _ = []
myZipWith f (x : xs) (y : ys) = f x y : myZipWith f xs ys

-- myZipWith3 - применение функции трех аргументов к трем спискам

myZipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
myZipWith3 _ [] _ _ = []
myZipWith3 _ _ [] _ = []
myZipWith3 _ _ _ [] = []
myZipWith3 f (x : xs) (y : ys) (z : zs) = f x y z : myZipWith3 f xs ys zs

-- myAll - проверяет удовлетворяют ли все элементы списка предикату

myAll :: (a -> Bool) -> [a] -> Bool
myAll predicate [] = True
myAll predicate (x : xs) = predicate x && myAll predicate xs

-- myAny - проверяет удовлетворяют ли хотя бы один элемент списка предикату

myAny :: (a -> Bool) -> [a] -> Bool
myAny predicate [] = False
myAny predicate (x : xs) = predicate x || myAny predicate xs

-- myComposition - композиция двух функций (.)

myComposition :: (b -> c) -> (a -> b) -> a -> c
myComposition f g x = f (g x)