-------------------------------------
-- Практические задание 1. Часть 2 --
-------------------------------------

module Pr01_2 where

{-

Напишите реализацию функций:
-- myZipSave - попарное объединение двух списков в список пар и сохранение хвоста более длинного списка 
-- myUnzipSave - разделение списка пар на пару списков с восстановлением более длинного списка если исходные списки были разного размера
-- myReverse - разворот списка с использованием сверток
-- myFoldl1 - левая свертка для не пустых списков (без инициирующего значения)
-- myFoldr1 - правая свертка для не пустых списков (без инициирующего значения)
-- myTakeWhile - реализовать с использованием сверток
-- mySpan - реализовать с использованием сверток
-- myMaybe - обработка возможно отсутствующего значения или возвращение значение по умолчанию (maybe)
-- myMap - реализуйте функцию map с использованием типа MyList из материалов лекции
-- myUnFoldr - развертка (операция обратная к свертке)

-- Расширьте типы для выпекания тортов из материалов лекции:
    -- Добавить возможность испечь не менее трех типов тортов
    -- Контроль числа и объема используемых ингредиентов
    -- Обработку недостатка или отсутствия ингредиентов

-}

-- myZipSave - попарное объединение двух списков в список пар и сохранение хвоста более длинного списка 

myZipSave :: [a] -> [b] -> ([(a, b)], Either [a] [b])
myZipSave [] [] = ([], Left [])
myZipSave xs [] = ([], Left xs)
myZipSave [] ys = ([], Right ys)
myZipSave (x:xs) (y:ys) =
    let (pairs, tail) = myZipSave xs ys
    in ((x,y) : pairs, tail)

-- myUnzipSave - разделение списка пар на пару списков с восстановлением более длинного списка если исходные списки были разного размера

myUnzipSave :: ([(a, b)], Either [a] [b]) -> ([a],[b])
myUnzipSave ([], Left []) = ([], [])
myUnzipSave ([], Left xs) = (xs, [])
myUnzipSave ([], Right ys) = ([], ys)
myUnzipSave ((x,y):xys, tail) = 
    let (xs, ys) = myUnzipSave (xys, tail)
    in (x:xs, y:ys)

-- myReverse - разворот списка с использованием сверток

myReverse :: [a] -> [a]
myReverse xs = foldl (flip(:)) [] xs

-- myFoldl1 - левая свертка для не пустых списков (без инициирующего значения)

myFoldl1 :: (a -> a -> a) -> [a] -> a
myFoldl1 f (x:xs) = foldl f x xs

-- myFoldr1 - правая свертка для не пустых списков (без инициирующего значения)

myFoldr1 :: (a -> a -> a) -> [a] -> a
myFoldr1 f [x] = x
myFoldr1 f (x:xs) = f x (myFoldr1 f xs)

-- myTakeWhile - реализовать с использованием сверток

myTakenWhile :: (a -> Bool) -> [a] -> [a]
myTakenWhile pred xs = foldr func [] xs
    where func x acc = if pred x then x : acc else []

-- mySpan - реализовать с использованием сверток

mySpan :: (a -> Bool) -> [a] -> ([a], [a])
mySpan pred xs = foldr func ([], []) xs
    where
        func x (true, false)
            | pred x = (x: true, false)
            | otherwise = ([], x: true ++ false)

mySpan' :: (a -> Bool) -> [a] -> ([a], [a])
mySpan' pred xs = foldr func ([], []) xs
    where
        func x acc = 
            let (true, false) = acc
            in if pred x 
                then (x : true, false)
                else ([], x : true ++ false)


-- myMaybe - обработка возможно отсутствующего значения или возвращение значение по умолчанию (maybe)

myMaybe :: b -> (a -> b) -> Maybe a -> b
myMaybe x _ Nothing = x
myMaybe _ f (Just y) = f y

-- myMap - реализуйте функцию map с использованием типа MyList из материалов лекции

data MyList a = MyEmpty | MyCons a (MyList a) deriving Show

myMap :: (a -> b) -> MyList a -> MyList b
myMap _ MyEmpty = MyEmpty
myMap f (MyCons x xs) = MyCons (f x) (myMap f xs)


-- myUnFoldr - развертка (операция обратная к свертке)

myUnFoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnFoldr f value = doing (f value)
    where
        doing Nothing = []
        doing (Just (x, newValue)) = x : myUnFoldr f newValue

{-
-- Расширьте типы для выпекания тортов из материалов лекции:
    -- Добавить возможность испечь не менее трех типов тортов
    -- Контроль числа и объема используемых ингредиентов
    -- Обработку недостатка или отсутствия ингредиентов
-}

newtype Grams = Grams Double deriving (Show)
newtype Pieces = Pieces Int deriving (Show)
newtype Milliliters = Milliliters Double deriving (Show)

data Ingredient = Oil Grams
                | Chocolate Grams
                | Egg Pieces
                | Flour Grams
                | Sugar Grams
                | BakingPowder Grams
                | Milk Milliliters
                | Cream Milliliters
                | Lemon Grams
                | LemonZest Grams
                | Matcha Grams
                | CreamCheese Grams
                | Honey Grams
                | CondensedMilk Milliliters
                | Butter Grams
                | Kefir Milliliters
                | Cocoa Grams
                | FoodColoring String
                | Vinegar Milliliters
                | Salt Grams
                | VanillaSugar Grams
                deriving (Show)

data CakeType = ChocolateCake
              | LemonCake
              | MatchaCheesecake
              | HoneyCake
              | RedVelvet
              deriving (Show, Eq)

data Dough = PlainDough
           | ChocolateDough
           | LemonDough
           | MatchaDough
           | HoneyDough
           | RedVelvetDough
           deriving (Show)

data Cake = Cake CakeType deriving (Show)

data Oven = Oven Int deriving (Show)

myIsNothing :: Maybe a -> Bool
myIsNothing Nothing = True
myIsNothing _ = False

myFromJust :: Maybe a -> a
myFromJust (Just x) = x
myFromJust Nothing = error "myFromJust: Nothing"

findIngredient :: (Ingredient -> Bool) -> [Ingredient] -> Maybe Ingredient
findIngredient _ [] = Nothing
findIngredient p (x:xs) =
    if p x
        then Just x
        else findIngredient p xs

isOil (Oil _) = True
isOil _ = False
isChocolate (Chocolate _) = True
isChocolate _ = False
isEgg (Egg _) = True
isEgg _ = False
isFlour (Flour _) = True
isFlour _ = False
isSugar (Sugar _) = True
isSugar _ = False
isBakingPowder (BakingPowder _) = True
isBakingPowder _ = False
isMilk (Milk _) = True
isMilk _ = False
isCream (Cream _) = True
isCream _ = False
isLemon (Lemon _) = True
isLemon _ = False
isLemonZest (LemonZest _) = True
isLemonZest _ = False
isMatcha (Matcha _) = True
isMatcha _ = False
isCreamCheese (CreamCheese _) = True
isCreamCheese _ = False
isHoney (Honey _) = True
isHoney _ = False
isCondensedMilk (CondensedMilk _) = True
isCondensedMilk _ = False
isButter (Butter _) = True
isButter _ = False
isKefir (Kefir _) = True
isKefir _ = False
isCocoa (Cocoa _) = True
isCocoa _ = False
isFoodColoring (FoodColoring _) = True
isFoodColoring _ = False
isVinegar (Vinegar _) = True
isVinegar _ = False
isSalt (Salt _) = True
isSalt _ = False
isVanillaSugar (VanillaSugar _) = True
isVanillaSugar _ = False

makeChocolateCake :: [Ingredient] -> Either String (Cake, String)
makeChocolateCake ingredients =
    let oilCheck = findIngredient isOil ingredients
    in if myIsNothing oilCheck
        then Left "ERROR: missing Oil"
        else let (Oil (Grams g)) = myFromJust oilCheck
             in if g < 150
                then Left ("Oil: need 150, have " ++ show g ++ " (not enough)")
                else
    let chocCheck = findIngredient isChocolate ingredients
    in if myIsNothing chocCheck
        then Left "ERROR: missing Chocolate"
        else let (Chocolate (Grams g2)) = myFromJust chocCheck
             in if g2 < 200
                then Left ("Chocolate: need 200, have " ++ show g2 ++ " (not enough)")
                else
    let eggCheck = findIngredient isEgg ingredients
    in if myIsNothing eggCheck
        then Left "ERROR: missing Eggs"
        else let (Egg (Pieces p)) = myFromJust eggCheck
             in if p < 3
                then Left ("Eggs: need 3, have " ++ show p ++ " (not enough)")
                else
    let flourCheck = findIngredient isFlour ingredients
    in if myIsNothing flourCheck
        then Left "ERROR: missing Flour"
        else let (Flour (Grams g3)) = myFromJust flourCheck
             in if g3 < 250
                then Left ("Flour: need 250, have " ++ show g3 ++ " (not enough)")
                else
    let sugarCheck = findIngredient isSugar ingredients
    in if myIsNothing sugarCheck
        then Left "ERROR: missing Sugar"
        else let (Sugar (Grams g4)) = myFromJust sugarCheck
             in if g4 < 200
                then Left ("Sugar: need 200, have " ++ show g4 ++ " (not enough)")
                else
    let bakingCheck = findIngredient isBakingPowder ingredients
    in if myIsNothing bakingCheck
        then Left "ERROR: missing Baking Powder"
        else let (BakingPowder (Grams g5)) = myFromJust bakingCheck
             in if g5 < 10
                then Left ("Baking Powder: need 10, have " ++ show g5 ++ " (not enough)")
                else
    let milkCheck = findIngredient isMilk ingredients
    in if myIsNothing milkCheck
        then Left "ERROR: missing Milk"
        else let (Milk (Milliliters ml)) = myFromJust milkCheck
             in if ml < 200
                then Left ("Milk: need 200, have " ++ show ml ++ " (not enough)")
                else
    let excess = 
            (if g > 150 then "Oil: +" ++ show (g - 150) ++ " " else "") ++
            (if g2 > 200 then "Chocolate: +" ++ show (g2 - 200) ++ " " else "") ++
            (if p > 3 then "Eggs: +" ++ show (p - 3) ++ " " else "") ++
            (if g3 > 250 then "Flour: +" ++ show (g3 - 250) ++ " " else "") ++
            (if g4 > 200 then "Sugar: +" ++ show (g4 - 200) ++ " " else "") ++
            (if g5 > 10 then "Baking Powder: +" ++ show (g5 - 10) ++ " " else "") ++
            (if ml > 200 then "Milk: +" ++ show (ml - 200) ++ " " else "")
    in if excess == ""
        then Right (Cake ChocolateCake, "Ingredients exactly as needed")
        else Right (Cake ChocolateCake, "EXCESS: " ++ excess)

makeLemonCake :: [Ingredient] -> Either String (Cake, String)
makeLemonCake ingredients =
    let flourCheck = findIngredient isFlour ingredients
    in if myIsNothing flourCheck
        then Left "ERROR: missing Flour"
        else let (Flour (Grams g)) = myFromJust flourCheck
             in if g < 250
                then Left ("Flour: need 250, have " ++ show g ++ " (not enough)")
                else
    let sugarCheck = findIngredient isSugar ingredients
    in if myIsNothing sugarCheck
        then Left "ERROR: missing Sugar"
        else let (Sugar (Grams g2)) = myFromJust sugarCheck
             in if g2 < 200
                then Left ("Sugar: need 200, have " ++ show g2 ++ " (not enough)")
                else
    let eggCheck = findIngredient isEgg ingredients
    in if myIsNothing eggCheck
        then Left "ERROR: missing Eggs"
        else let (Egg (Pieces p)) = myFromJust eggCheck
             in if p < 3
                then Left ("Eggs: need 3, have " ++ show p ++ " (not enough)")
                else
    let butterCheck = findIngredient isButter ingredients
    in if myIsNothing butterCheck
        then Left "ERROR: missing Butter"
        else let (Butter (Grams g3)) = myFromJust butterCheck
             in if g3 < 150
                then Left ("Butter: need 150, have " ++ show g3 ++ " (not enough)")
                else
    let lemonCheck = findIngredient isLemon ingredients
    in if myIsNothing lemonCheck
        then Left "ERROR: missing Lemon"
        else let (Lemon (Grams g4)) = myFromJust lemonCheck
             in if g4 < 100
                then Left ("Lemon: need 100, have " ++ show g4 ++ " (not enough)")
                else
    let zestCheck = findIngredient isLemonZest ingredients
    in if myIsNothing zestCheck
        then Left "ERROR: missing Lemon Zest"
        else let (LemonZest (Grams g5)) = myFromJust zestCheck
             in if g5 < 20
                then Left ("Lemon Zest: need 20, have " ++ show g5 ++ " (not enough)")
                else
    let bakingCheck = findIngredient isBakingPowder ingredients
    in if myIsNothing bakingCheck
        then Left "ERROR: missing Baking Powder"
        else let (BakingPowder (Grams g6)) = myFromJust bakingCheck
             in if g6 < 10
                then Left ("Baking Powder: need 10, have " ++ show g6 ++ " (not enough)")
                else
    let excess = 
            (if g > 250 then "Flour: +" ++ show (g - 250) ++ " " else "") ++
            (if g2 > 200 then "Sugar: +" ++ show (g2 - 200) ++ " " else "") ++
            (if p > 3 then "Eggs: +" ++ show (p - 3) ++ " " else "") ++
            (if g3 > 150 then "Butter: +" ++ show (g3 - 150) ++ " " else "") ++
            (if g4 > 100 then "Lemon: +" ++ show (g4 - 100) ++ " " else "") ++
            (if g5 > 20 then "Lemon Zest: +" ++ show (g5 - 20) ++ " " else "") ++
            (if g6 > 10 then "Baking Powder: +" ++ show (g6 - 10) ++ " " else "")
    in if excess == ""
        then Right (Cake LemonCake, "Ingredients exactly as needed")
        else Right (Cake LemonCake, "EXCESS: " ++ excess)

makeMatchaCheesecake :: [Ingredient] -> Either String (Cake, String)
makeMatchaCheesecake ingredients =
    let cheeseCheck = findIngredient isCreamCheese ingredients
    in if myIsNothing cheeseCheck
        then Left "ERROR: missing Cream Cheese"
        else let (CreamCheese (Grams g)) = myFromJust cheeseCheck
             in if g < 500
                then Left ("Cream Cheese: need 500, have " ++ show g ++ " (not enough)")
                else
    let sugarCheck = findIngredient isSugar ingredients
    in if myIsNothing sugarCheck
        then Left "ERROR: missing Sugar"
        else let (Sugar (Grams g2)) = myFromJust sugarCheck
             in if g2 < 150
                then Left ("Sugar: need 150, have " ++ show g2 ++ " (not enough)")
                else
    let eggCheck = findIngredient isEgg ingredients
    in if myIsNothing eggCheck
        then Left "ERROR: missing Eggs"
        else let (Egg (Pieces p)) = myFromJust eggCheck
             in if p < 2
                then Left ("Eggs: need 2, have " ++ show p ++ " (not enough)")
                else
    let matchaCheck = findIngredient isMatcha ingredients
    in if myIsNothing matchaCheck
        then Left "ERROR: missing Matcha"
        else let (Matcha (Grams g3)) = myFromJust matchaCheck
             in if g3 < 30
                then Left ("Matcha: need 30, have " ++ show g3 ++ " (not enough)")
                else
    let butterCheck = findIngredient isButter ingredients
    in if myIsNothing butterCheck
        then Left "ERROR: missing Butter"
        else let (Butter (Grams g4)) = myFromJust butterCheck
             in if g4 < 100
                then Left ("Butter: need 100, have " ++ show g4 ++ " (not enough)")
                else
    let flourCheck = findIngredient isFlour ingredients
    in if myIsNothing flourCheck
        then Left "ERROR: missing Flour"
        else let (Flour (Grams g5)) = myFromJust flourCheck
             in if g5 < 50
                then Left ("Flour: need 50, have " ++ show g5 ++ " (not enough)")
                else
    let excess = 
            (if g > 500 then "Cream Cheese: +" ++ show (g - 500) ++ " " else "") ++
            (if g2 > 150 then "Sugar: +" ++ show (g2 - 150) ++ " " else "") ++
            (if p > 2 then "Eggs: +" ++ show (p - 2) ++ " " else "") ++
            (if g3 > 30 then "Matcha: +" ++ show (g3 - 30) ++ " " else "") ++
            (if g4 > 100 then "Butter: +" ++ show (g4 - 100) ++ " " else "") ++
            (if g5 > 50 then "Flour: +" ++ show (g5 - 50) ++ " " else "")
    in if excess == ""
        then Right (Cake MatchaCheesecake, "Ingredients exactly as needed")
        else Right (Cake MatchaCheesecake, "EXCESS: " ++ excess)

makeHoneyCake :: [Ingredient] -> Either String (Cake, String)
makeHoneyCake ingredients =
    let flourCheck = findIngredient isFlour ingredients
    in if myIsNothing flourCheck
        then Left "ERROR: missing Flour"
        else let (Flour (Grams g)) = myFromJust flourCheck
             in if g < 400
                then Left ("Flour: need 400, have " ++ show g ++ " (not enough)")
                else
    let sugarCheck = findIngredient isSugar ingredients
    in if myIsNothing sugarCheck
        then Left "ERROR: missing Sugar"
        else let (Sugar (Grams g2)) = myFromJust sugarCheck
             in if g2 < 150
                then Left ("Sugar: need 150, have " ++ show g2 ++ " (not enough)")
                else
    let eggCheck = findIngredient isEgg ingredients
    in if myIsNothing eggCheck
        then Left "ERROR: missing Eggs"
        else let (Egg (Pieces p)) = myFromJust eggCheck
             in if p < 2
                then Left ("Eggs: need 2, have " ++ show p ++ " (not enough)")
                else
    let butterCheck = findIngredient isButter ingredients
    in if myIsNothing butterCheck
        then Left "ERROR: missing Butter"
        else let (Butter (Grams g3)) = myFromJust butterCheck
             in if g3 < 100
                then Left ("Butter: need 100, have " ++ show g3 ++ " (not enough)")
                else
    let honeyCheck = findIngredient isHoney ingredients
    in if myIsNothing honeyCheck
        then Left "ERROR: missing Honey"
        else let (Honey (Grams g4)) = myFromJust honeyCheck
             in if g4 < 150
                then Left ("Honey: need 150, have " ++ show g4 ++ " (not enough)")
                else
    let bakingCheck = findIngredient isBakingPowder ingredients
    in if myIsNothing bakingCheck
        then Left "ERROR: missing Baking Powder"
        else let (BakingPowder (Grams g5)) = myFromJust bakingCheck
             in if g5 < 10
                then Left ("Baking Powder: need 10, have " ++ show g5 ++ " (not enough)")
                else
    let vanillaCheck = findIngredient isVanillaSugar ingredients
    in if myIsNothing vanillaCheck
        then Left "ERROR: missing Vanilla Sugar"
        else let (VanillaSugar (Grams g6)) = myFromJust vanillaCheck
             in if g6 < 10
                then Left ("Vanilla Sugar: need 10, have " ++ show g6 ++ " (not enough)")
                else
    let saltCheck = findIngredient isSalt ingredients
    in if myIsNothing saltCheck
        then Left "ERROR: missing Salt"
        else let (Salt (Grams g7)) = myFromJust saltCheck
             in if g7 < 5
                then Left ("Salt: need 5, have " ++ show g7 ++ " (not enough)")
                else
    let excess = 
            (if g > 400 then "Flour: +" ++ show (g - 400) ++ " " else "") ++
            (if g2 > 150 then "Sugar: +" ++ show (g2 - 150) ++ " " else "") ++
            (if p > 2 then "Eggs: +" ++ show (p - 2) ++ " " else "") ++
            (if g3 > 100 then "Butter: +" ++ show (g3 - 100) ++ " " else "") ++
            (if g4 > 150 then "Honey: +" ++ show (g4 - 150) ++ " " else "") ++
            (if g5 > 10 then "Baking Powder: +" ++ show (g5 - 10) ++ " " else "") ++
            (if g6 > 10 then "Vanilla Sugar: +" ++ show (g6 - 10) ++ " " else "") ++
            (if g7 > 5 then "Salt: +" ++ show (g7 - 5) ++ " " else "")
    in if excess == ""
        then Right (Cake HoneyCake, "Ingredients exactly as needed")
        else Right (Cake HoneyCake, "EXCESS: " ++ excess)

makeRedVelvet :: [Ingredient] -> Either String (Cake, String)
makeRedVelvet ingredients =
    let flourCheck = findIngredient isFlour ingredients
    in if myIsNothing flourCheck
        then Left "ERROR: missing Flour"
        else let (Flour (Grams g)) = myFromJust flourCheck
             in if g < 300
                then Left ("Flour: need 300, have " ++ show g ++ " (not enough)")
                else
    let sugarCheck = findIngredient isSugar ingredients
    in if myIsNothing sugarCheck
        then Left "ERROR: missing Sugar"
        else let (Sugar (Grams g2)) = myFromJust sugarCheck
             in if g2 < 250
                then Left ("Sugar: need 250, have " ++ show g2 ++ " (not enough)")
                else
    let eggCheck = findIngredient isEgg ingredients
    in if myIsNothing eggCheck
        then Left "ERROR: missing Eggs"
        else let (Egg (Pieces p)) = myFromJust eggCheck
             in if p < 2
                then Left ("Eggs: need 2, have " ++ show p ++ " (not enough)")
                else
    let butterCheck = findIngredient isButter ingredients
    in if myIsNothing butterCheck
        then Left "ERROR: missing Butter"
        else let (Butter (Grams g3)) = myFromJust butterCheck
             in if g3 < 100
                then Left ("Butter: need 100, have " ++ show g3 ++ " (not enough)")
                else
    let kefirCheck = findIngredient isKefir ingredients
    in if myIsNothing kefirCheck
        then Left "ERROR: missing Kefir"
        else let (Kefir (Milliliters ml)) = myFromJust kefirCheck
             in if ml < 250
                then Left ("Kefir: need 250, have " ++ show ml ++ " (not enough)")
                else
    let cocoaCheck = findIngredient isCocoa ingredients
    in if myIsNothing cocoaCheck
        then Left "ERROR: missing Cocoa"
        else let (Cocoa (Grams g4)) = myFromJust cocoaCheck
             in if g4 < 20
                then Left ("Cocoa: need 20, have " ++ show g4 ++ " (not enough)")
                else
    let colorCheck = findIngredient isFoodColoring ingredients
    in if myIsNothing colorCheck
        then Left "ERROR: missing Red Food Coloring"
        else
    let vinegarCheck = findIngredient isVinegar ingredients
    in if myIsNothing vinegarCheck
        then Left "ERROR: missing Vinegar"
        else let (Vinegar (Milliliters ml2)) = myFromJust vinegarCheck
             in if ml2 < 10
                then Left ("Vinegar: need 10, have " ++ show ml2 ++ " (not enough)")
                else
    let bakingCheck = findIngredient isBakingPowder ingredients
    in if myIsNothing bakingCheck
        then Left "ERROR: missing Baking Powder"
        else let (BakingPowder (Grams g5)) = myFromJust bakingCheck
             in if g5 < 10
                then Left ("Baking Powder: need 10, have " ++ show g5 ++ " (not enough)")
                else
    let saltCheck = findIngredient isSalt ingredients
    in if myIsNothing saltCheck
        then Left "ERROR: missing Salt"
        else let (Salt (Grams g6)) = myFromJust saltCheck
             in if g6 < 3
                then Left ("Salt: need 3, have " ++ show g6 ++ " (not enough)")
                else
    let excess = 
            (if g > 300 then "Flour: +" ++ show (g - 300) ++ " " else "") ++
            (if g2 > 250 then "Sugar: +" ++ show (g2 - 250) ++ " " else "") ++
            (if p > 2 then "Eggs: +" ++ show (p - 2) ++ " " else "") ++
            (if g3 > 100 then "Butter: +" ++ show (g3 - 100) ++ " " else "") ++
            (if ml > 250 then "Kefir: +" ++ show (ml - 250) ++ " " else "") ++
            (if g4 > 20 then "Cocoa: +" ++ show (g4 - 20) ++ " " else "") ++
            (if ml2 > 10 then "Vinegar: +" ++ show (ml2 - 10) ++ " " else "") ++
            (if g5 > 10 then "Baking Powder: +" ++ show (g5 - 10) ++ " " else "") ++
            (if g6 > 3 then "Salt: +" ++ show (g6 - 3) ++ " " else "")
    in if excess == ""
        then Right (Cake RedVelvet, "Ingredients exactly as needed")
        else Right (Cake RedVelvet, "EXCESS: " ++ excess)

bakeCake :: Dough -> Oven -> Either String Cake
bakeCake ChocolateDough (Oven temp) =
    if temp >= 170 && temp <= 180
        then Right (Cake ChocolateCake)
        else Left "ERROR: wrong temperature for chocolate cake"
bakeCake LemonDough (Oven temp) =
    if temp >= 160 && temp <= 175
        then Right (Cake LemonCake)
        else Left "ERROR: wrong temperature for lemon cake"
bakeCake MatchaDough (Oven temp) =
    if temp >= 150 && temp <= 160
        then Right (Cake MatchaCheesecake)
        else Left "ERROR: wrong temperature for matcha cheesecake"
bakeCake HoneyDough (Oven temp) =
    if temp >= 170 && temp <= 180
        then Right (Cake HoneyCake)
        else Left "ERROR: wrong temperature for honey cake"
bakeCake RedVelvetDough (Oven temp) =
    if temp >= 160 && temp <= 175
        then Right (Cake RedVelvet)
        else Left "ERROR: wrong temperature for red velvet"
bakeCake _ _ = Left "ERROR: unknown dough type"

-- Проверка

chocoIngs = [Oil (Grams 200), Chocolate (Grams 250), Egg (Pieces 2), 
             Flour (Grams 300), Sugar (Grams 220), BakingPowder (Grams 15), 
             Milk (Milliliters 250)]
chocoResult = makeChocolateCake chocoIngs

lemonIngs = [Flour (Grams 300), Sugar (Grams 220), Egg (Pieces 4), 
             Butter (Grams 200), Lemon (Grams 150), LemonZest (Grams 30), 
             BakingPowder (Grams 15)]
lemonResult = makeLemonCake lemonIngs

matchaIngs = [CreamCheese (Grams 600), Sugar (Grams 180), Egg (Pieces 3), 
              Matcha (Grams 40), Butter (Grams 120), Flour (Grams 70)]
matchaResult = makeMatchaCheesecake matchaIngs

honeyIngs = [Flour (Grams 450), Sugar (Grams 180), Egg (Pieces 3), 
             Butter (Grams 150), Honey (Grams 180), BakingPowder (Grams 15), 
             VanillaSugar (Grams 15), Salt (Grams 7)]
honeyResult = makeHoneyCake honeyIngs

redVelvetIngs = [Flour (Grams 350), Sugar (Grams 280), Egg (Pieces 3), 
                 Butter (Grams 150), Kefir (Milliliters 300), Cocoa (Grams 25), 
                 FoodColoring "Red", Vinegar (Milliliters 15), 
                 BakingPowder (Grams 15), Salt (Grams 5)]
redResult = makeRedVelvet redVelvetIngs


ffun :: Maybe [a -> b] -> Maybe [a] -> Maybe [b]
ffun xs Nothing = Nothing
ffun Nothing ys = Nothing
ffun (Just xs) (Just ys) =  Just (doing xs ys)
    where doing [] _ = []
          doing _ [] = []
          doing (f:fs) (k:ks) = f k : doing fs ks


ffun' :: Either [a -> b] [c] -> Either [a] [c] -> Either [b] [c]
ffun' xs (Right r) = Right r
ffun' (Right r) ys = Right r
ffun' (Left xs) (Left ys) =  Left (doing xs ys)
    where doing [] _ = []
          doing _ [] = []
          doing (f:fs) (k:ks) = f k : doing fs ks

-- data Maybe a    = Nothing | Just a
-- data Either a b =  Left a | Right b