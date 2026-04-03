module MyTypes.MyMaybe (MyMaybe(..)) where

data MyMaybe a = MyNothing | MyJust a deriving (Show, Eq, Ord)

instance Foldable MyMaybe where
    foldr _ z MyNothing = z
    foldr f z (MyJust x) = f x z

instance Semigroup a => Semigroup (MyMaybe a) where
    MyNothing <> b = b
    a <> MyNothing = a
    (MyJust x) <> (MyJust y) = MyJust (x <> y)

instance Semigroup a => Monoid (MyMaybe a) where
    mempty = MyNothing
    mappend = (<>)

instance Functor MyMaybe where
    fmap _ MyNothing = MyNothing
    fmap f (MyJust x) = MyJust (f x)

instance Applicative MyMaybe where
    pure = MyJust
    
    MyNothing <*> _ = MyNothing
    _ <*> MyNothing = MyNothing
    (MyJust f) <*> (MyJust x) = MyJust (f x)

{- Примеры вызова функций из ghci:

1. Foldable (fold, foldMap, foldr):
   let maybe1 = MyJust 42
   let maybe2 = MyNothing :: MyMaybe Int
   
   -- foldr
   foldr (+) 0 maybe1   -- 42
   foldr (+) 0 maybe2   -- 0
   
   -- foldMap
   foldMap show maybe1  -- "42"
   foldMap show maybe2  -- ""
   
   -- fold
   fold (MyJust [1,2,3])  -- [1,2,3]
   fold (MyNothing :: MyMaybe [Int])  -- []

2. Semigroup ((<>), sconcat, stimes):
   import qualified Data.List.NonEmpty as NE
   let mj1 = MyJust [1,2,3]
   let mj2 = MyJust [4,5,6]
   let mn = MyNothing
   
   -- (<>)
   mj1 <> mj2   -- MyJust [1,2,3,4,5,6]
   mj1 <> mn    -- MyJust [1,2,3]
   mn <> mj1    -- MyJust [1,2,3]
   
   -- sconcat
   import Data.Semigroup (sconcat)
   sconcat (mj1 NE.:| [mj2, MyJust [7,8]])  -- MyJust [1,2,3,4,5,6,7,8]
   
   -- stimes
   import Data.Semigroup (stimes)
   stimes 3 mj1  -- MyJust [1,2,3,1,2,3,1,2,3]

3. Monoid (mappend, mconcat):
   -- mappend (то же что и <>)
   mappend mj1 mj2  -- MyJust [1,2,3,4,5,6]
   
   -- mconcat
   mconcat [mj1, mn, mj2, MyJust [9]]  -- MyJust [1,2,3,4,5,6,9]

4. Functor (fmap, (<$)):
   -- fmap
   fmap (*3) (MyJust 7)   -- MyJust 21
   fmap (*3) MyNothing    -- MyNothing
   
   -- (<$)
   1000 <$ (MyJust 7)     -- MyJust 1000
   ("new" <$) MyNothing   -- MyNothing

5. Applicative (pure, (<*>), liftA2, (*>), (<*)):
   -- pure
   pure 777 :: MyMaybe Int  -- MyJust 777
   
   -- (<*>)
   let mf = MyJust (*4)
   mf <*> (MyJust 11)      -- MyJust 44
   mf <*> MyNothing        -- MyNothing
   MyNothing <*> (MyJust 5)  -- MyNothing
   
   -- liftA2
   liftA2 (+) (MyJust 13) (MyJust 37)   -- MyJust 50
   liftA2 (+) MyNothing (MyJust 37)     -- MyNothing
   
   -- (*>)
   (MyJust 5) *> (MyJust 9)   -- MyJust 9
   MyNothing *> (MyJust 9)    -- MyNothing
   
   -- (<*)
   (MyJust 5) <* (MyJust 9)   -- MyJust 5
   MyNothing <* (MyJust 9)    -- MyNothing
-}