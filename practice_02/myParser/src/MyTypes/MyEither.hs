module MyTypes.MyEither (MyEither(..)) where

data MyEither a b = MyLeft a | MyRight b deriving (Show, Eq, Ord)

instance Functor (MyEither a) where
    fmap _ (MyLeft x) = MyLeft x
    fmap f (MyRight y) = MyRight (f y)

instance Foldable (MyEither a) where
    foldr _ z (MyLeft _) = z
    foldr f z (MyRight y) = f y z
    
    foldMap _ (MyLeft _) = mempty
    foldMap f (MyRight y) = f y

instance Semigroup b => Semigroup (MyEither a b) where
    (MyLeft x) <> _ = MyLeft x
    _ <> (MyLeft y) = MyLeft y
    (MyRight x) <> (MyRight y) = MyRight (x <> y)

instance Applicative (MyEither a) where
    pure = MyRight
    
    (MyLeft x) <*> _ = MyLeft x
    _ <*> (MyLeft y) = MyLeft y
    (MyRight f) <*> (MyRight x) = MyRight (f x)

{-
Примеры вызова функций из ghci:

1. Foldable:
   let right1 = MyRight 228 :: MyEither String Int
   let left1 = MyLeft "FAIL" :: MyEither String Int
   foldr (+) 0 right1
   -- 228
   foldr (+) 0 left1
   -- 0
   foldMap show right1
   -- "228"

2. Semigroup:
   import qualified Data.List.NonEmpty as NE
   let er1 = MyRight [1,1,2] :: MyEither String [Int]
   let er2 = MyRight [3,5,8] :: MyEither String [Int]
   let el = MyLeft "CRASH" :: MyEither String [Int]
   er1 <> er2
   -- MyRight [1,1,2,3,5,8]
   er1 <> el
   -- MyLeft "CRASH"
   sconcat (er1 NE.:| [er2, MyRight [13,21]])
   -- MyRight [1,1,2,3,5,8,13,21]
   stimes 3 er1
   -- MyRight [1,1,2,1,1,2,1,1,2]

3. Functor:
   fmap (*10) (MyRight 42)
   -- MyRight 420
   fmap (*10) (MyLeft "NO")
   -- MyLeft "NO"
   999 <$ (MyRight 42)
   -- MyRight 999

4. Applicative:
   pure 1337 :: MyEither String Int
   -- MyRight 1337
   let ef = MyRight (*3) :: MyEither String (Int -> Int)
   ef <*> (MyRight 50)
   -- MyRight 150
   ef <*> (MyLeft "BAD")
   -- MyLeft "BAD"
   liftA2 (+) (MyRight 123) (MyRight 321)
   -- MyRight 444
   (MyRight 7) *> (MyRight 13)
   -- MyRight 13
   (MyRight 7) <* (MyRight 13)
   -- MyRight 7
-}