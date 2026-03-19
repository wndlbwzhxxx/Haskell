module MyTypes.MyTree (MyTree(..)) where

data MyTree a = Leaf a | Node a (MyTree a) (MyTree a) deriving (Show, Eq)

instance Functor MyTree where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

instance Foldable MyTree where
    foldMap f (Leaf x) = f x
    foldMap f (Node x left right) = foldMap f left <> f x <> foldMap f right

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

{-
Примеры вызова функций из ghci:

1. Foldable:
   let tree1 = Node 42 (Leaf 17) (Node 99 (Leaf 33) (Leaf 76))
   foldr (+) 0 tree1
   -- 267 (42+17+99+33+76)
   foldMap show tree1
   -- "4217993376"
   import Data.Monoid (Sum(..))
   foldMap Sum tree1
   -- Sum {getSum = 267}

2. Functor:
   let tree2 = Node 13 (Leaf 7) (Leaf 666)
   fmap (*2) tree2
   -- Node 26 (Leaf 14) (Leaf 1332)
   100500 <$ tree2
   -- Node 100500 (Leaf 100500) (Leaf 100500)

3. Applicative:
   let tree3 = Node (*2) (Leaf (+100)) (Leaf (*10))
   let tree4 = Node 8 (Leaf 3) (Leaf 15)
   pure 1337 :: MyTree Int
   -- Leaf 1337
   tree3 <*> tree4
   -- Node 16 (Leaf 103) (Leaf 150)
   liftA2 (+) (Leaf 228) (Leaf 322)
   -- Leaf 550
   tree3 *> tree4
   -- Node 8 (Leaf 3) (Leaf 15)
   tree3 <* tree4
   -- Node (*2) (Leaf (+100)) (Leaf (*10))
-}