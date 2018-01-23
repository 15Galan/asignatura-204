-------------------------------------------------------------------------------
-- Linear implementation of Sets. Nodes are sorted and non-repeated
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module DataStructures.Set.SortedLinearSet 
  ( Set
  , empty
  , isEmpty
  , insert
  , isElem
  , delete
  ) where

import Data.List(intercalate)
import Test.QuickCheck

data Set a = Empty | Node a (Set a)

empty :: Set a
empty  = Empty

isEmpty :: Set a -> Bool
isEmpty Empty  = True
isEmpty _      = False

insert :: (Ord a) => a -> Set a -> Set a
insert x Empty = Node x Empty
insert x (Node y s)
 | x < y       = Node x (Node y s)
 | x == y      = Node y s
 | otherwise   = Node y (insert x s)

isElem :: (Ord a) => a -> Set a -> Bool
isElem x Empty       = False
isElem x (Node y s)  
 | x < y      = False
 | otherwise  = x==y || isElem x s
 
delete :: (Ord a) => a -> Set a -> Set a
delete x Empty = Empty
delete x (Node y s)
 | x < y       = Node y s
 | x == y      = s
 | otherwise   = Node y (delete x s)

-- Showing a set
instance (Show a) => Show (Set a) where
  show s = "SortedLinearSet(" ++ intercalate "," (aux s) ++ ")"
    where
      aux Empty      =  []
      aux (Node x s) =  show x : aux s

-- set equality
instance (Eq a) => Eq (Set a) where
  Empty      == Empty         = True
  (Node x s) == (Node x' s')  = x==x' && s==s'
  _          == _             = False

-- This instace is used by QuickCheck to generate random sets
instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary =  do
      xs <- listOf arbitrary
      return (foldr insert empty xs)

