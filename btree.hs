
import Data.Monoid
import qualified Data.Foldable as F

data Btree a = Empty
             | Node a (Btree a) (Btree a) 
             deriving (Show, Eq)

instance Functor Btree where
    fmap f Empty = Empty
    fmap f (Node v l r) =  Node (f v) (fmap f l) (fmap f r)

instance F.Foldable Btree where
    foldMap f Empty = mempty
    foldMap f (Node v l r) = f v <> F.foldMap f l <> F.foldMap f r

data IntTree = EmptyNode
             | NodeInt Int IntTree IntTree 
             deriving (Show, Eq)

insertNode :: Ord a => a  -> Btree a -> Btree a
insertNode v Empty = Node v Empty Empty
insertNode v (Node x l r)  
        | v < x  = Node x (insertNode v l) r
        | v > x  = Node x l (insertNode v r)
        | otherwise = Node x l r


insertNodeInt :: Int  -> IntTree -> IntTree
insertNodeInt v EmptyNode = NodeInt v EmptyNode EmptyNode
insertNodeInt v (NodeInt x l r)  
        | v < x  = NodeInt x (insertNodeInt v l) r
        | v > x  = NodeInt x l (insertNodeInt v r)
        | otherwise = NodeInt x l r

fromListInt :: [Int] -> IntTree 
fromListInt = foldr insertNodeInt EmptyNode 

fromList :: Ord a => [a] -> Btree a
fromList = foldr insertNode Empty 

treeSize :: Btree a -> Int
treeSize Empty = 0
treeSize (Node x l r) = 1 + treeSize l + treeSize r


flatten :: Btree a -> [a]
flatten Empty = []
flatten (Node x l r) = (flatten l) ++ [x] ++ (flatten r)


sortTree :: Ord a => [a] -> [a]
sortTree = flatten . fromList

