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
  , size
  , insert
  , isElem
  , delete

  , fold

  , union
  , intersection
  , difference
  ) where

import Data.List(intercalate)
import Test.QuickCheck

data Set a  = Empty | Node a (Set a)

empty :: Set a
empty  = Empty

isEmpty :: Set a -> Bool
isEmpty Empty  = True
isEmpty _      = False

insert :: (Ord a) => a -> Set a -> Set a
insert x Empty  = Node x Empty
insert x (Node y s)
 | x < y        = Node x (Node y s)
 | x == y       = Node y s
 | otherwise    = Node y (insert x s)

isElem :: (Ord a) => a -> Set a -> Bool
isElem x Empty       = False
isElem x (Node y s)
 | x < y      = False
 | otherwise  = x==y || isElem x s

delete :: (Ord a) => a -> Set a -> Set a
delete x Empty  = Empty
delete x (Node y s)
 | x < y        = Node y s
 | x == y       = s
 | otherwise    = Node y (delete x s)

size :: Set a -> Int
size Empty       = 0
size (Node _ s)  = 1 + size s

fold :: (a -> b -> b) -> b -> Set a -> b
fold f z s = fun s
 where
  fun Empty       = z
  fun (Node x s)  = f x (fun s)

union :: (Ord a) => Set a -> Set a -> Set a
union s s'  = fold insert s s'

difference :: (Ord a) => Set a -> Set a -> Set a
difference s s'  = fold delete s s'

intersection :: (Ord a) => Set a -> Set a -> Set a
intersection s s'  = fold (\x inter -> if isElem x s then insert x inter else inter) empty s'

-- Showing a set
instance (Show a) => Show (Set a) where
  show s  = "SortedLinearSet(" ++ intercalate "," (aux s) ++ ")"
    where
      aux Empty       = []
      aux (Node x s)  = show x : aux s

-- Set equality
instance (Eq a) => Eq (Set a) where
  Empty      == Empty         = True
  (Node x s) == (Node x' s')  = x==x' && s==s'
  _          == _             = False

-- This instance is used by QuickCheck to generate random sets
instance (Ord a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary  = do
      xs <- listOf arbitrary
      return (foldr insert empty xs)
