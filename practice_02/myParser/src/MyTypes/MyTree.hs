module MyTypes.MyTree (MyTree(..)) where

data MyTree a = Leaf a | Node a (MyTree a) (MyTree a) deriving (Show, Eq)

instance Foldable MyTree where
    foldMap f (Leaf x) = f x
    foldMap f (Node x left right) = foldMap f left <> f x <> foldMap f right

instance Functor MyTree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

instance Applicative MyTree where
    pure x = Leaf x
    
    (Leaf f) <*> tree = fmap f tree
    (Node f leftf rightf) <*> tree = 
        Node (f (getValue tree)) 
             (leftf <*> tree) 
             (rightf <*> tree)
        where
            getValue (Leaf x) = x
            getValue (Node x _ _) = x

{- Примеры вызова функций из ghci:

1. Foldable (fold, foldMap, foldr):
   let tree1 = Node 42 (Leaf 17) (Node 99 (Leaf 33) (Leaf 76))
   let tree2 = Leaf 100
   
   -- foldr
   foldr (+) 0 tree1   -- 267
   foldr (+) 0 tree2   -- 100
   
   -- foldMap
   foldMap show tree1  -- "1742339976"
   foldMap show tree2  -- "100"
   
   -- fold
   import Data.Foldable
   fold (Node [1] (Leaf [2]) (Leaf [3]))  -- [2,1,3]

2. Functor (fmap, (<$)):
   let tree2 = Node 13 (Leaf 7) (Leaf 666)
   let leaf1 = Leaf 42
   
   -- fmap
   fmap (*2) tree2   -- Node 26 (Leaf 14) (Leaf 1332)
   fmap (*2) leaf1   -- Leaf 84
   
   -- (<$)
   100500 <$ tree2   -- Node 100500 (Leaf 100500) (Leaf 100500)
   "new" <$ leaf1    -- Leaf "new"

3. Applicative (pure, (<*>), liftA2, (*>), (<*)):
   let tree3 = Node (*2) (Leaf (+100)) (Leaf (*10))
   let tree4 = Node 8 (Leaf 3) (Leaf 15)
   let leafF = Leaf (+5)
   let leafVal = Leaf 10
   
   -- pure
   pure 1337 :: MyTree Int   -- Leaf 1337
   
   -- (<*>)
   tree3 <*> tree4   -- Node 16 (Node 108 (Leaf 103) (Leaf 115)) (Node 80 (Leaf 30) (Leaf 150))
   leafF <*> leafVal -- Leaf 15
   leafF <*> tree4   -- Node 13 (Leaf 8) (Leaf 20)
   
   -- liftA2
   liftA2 (+) (Leaf 228) (Leaf 322)           -- Leaf 550
   liftA2 (+) (Node 1 (Leaf 2) (Leaf 3)) (Node 10 (Leaf 20) (Leaf 30))
   -- Node 11 (Node 12 (Leaf 22) (Leaf 32)) (Node 13 (Leaf 23) (Leaf 33))
   
   -- (*>)
   tree3 *> tree4    -- Node 8 (Leaf 3) (Leaf 15)
   leafF *> leafVal  -- Leaf 10
   
   -- (<*)
   tree4 <* tree3    -- Node (*2) (Leaf (+100)) (Leaf (*10))
   leafVal <* leafF  -- Leaf (+5)
-}