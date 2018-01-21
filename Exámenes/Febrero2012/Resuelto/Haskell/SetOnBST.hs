-------------------------------------------------------------------------------
-- Sets as binary search trees
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2011
-------------------------------------------------------------------------------

module SetOnBST
  ( Set
  , empty
  , isEmpty
  , insert
  , isElem
  , notIsElem
  , delete
  , union
  , intersection
  , difference
  ) where

import Data.List(intercalate)
import Test.QuickCheck
import qualified BinarySearchTree as T

data Set a = S (T.BST a)

empty :: Set a
empty = S T.empty

isEmpty :: Set a -> Bool
isEmpty (S tree) = T.isEmpty tree

insert :: (Ord a) => a -> Set a -> Set a
insert x (S tree) = S (T.insert x tree) -- relies on BST not admiting repeated elements

isElem :: (Ord a) => a -> Set a -> Bool
isElem x (S tree) = T.isElem x tree

notIsElem :: (Ord a) => a -> Set a -> Bool
notIsElem x s = not (isElem x s)

delete :: (Ord a) => a -> Set a -> Set a
delete x (S tree) = S (T.delete x tree)

elems :: Set a -> [a]
elems (S tree) = T.inOrder tree

union :: (Ord a) => Set a -> Set a -> Set a
union s s' = foldr insert s (elems s')

difference :: (Ord a) => Set a -> Set a -> Set a
difference s s' = foldr delete s (elems s')

intersection :: (Ord a) => Set a -> Set a -> Set a
intersection s s' = foldr (\x inter -> if isElem x s then insert x inter else inter) empty (elems s')

-- Showing sets
instance (Show a) => Show (Set a) where
  show s = "Set(" ++ intercalate "," (map show $ elems s) ++ ")"

-- sets equiality
instance (Eq a) => Eq (Set a) where
        s == s' = elems s == elems s'
        
-- This instace is used by QuickCheck to generate random sets
instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
    arbitrary =  do
      xs <- listOf arbitrary
      return (foldr insert empty xs)
 

