-------------------------------------------------------------------------------
-- Implementation of Sets without repetition of elements, delegated on lists
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2015
-------------------------------------------------------------------------------

module DataStructures.Set.ListSet
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

import qualified Data.List
import Test.QuickCheck

data Set a  = St [a]

empty :: Set a
empty  = St []

isEmpty :: Set a -> Bool
isEmpty (St xs)  = null xs

insert :: (Eq a) => a -> Set a -> Set a
insert x s@(St xs)  = if elem x xs then s else St (x:xs)

isElem :: (Eq a) => a -> Set a -> Bool
isElem x (St xs)  = elem x xs

delete :: (Eq a) => a -> Set a -> Set a
delete x (St xs)  = St (Data.List.delete x xs)

size :: Set a -> Int
size (St xs)  = length xs

fold :: (a -> b -> b) -> b -> Set a -> b
fold f z  (St xs) = foldr f z xs

union :: (Ord a) => Set a -> Set a -> Set a
union s s'  = fold insert s s'

difference :: (Ord a) => Set a -> Set a -> Set a
difference s s'  = fold delete s s'

intersection :: (Ord a) => Set a -> Set a -> Set a
intersection s s'  = fold (\x inter -> if isElem x s then insert x inter else inter) empty s'

-- Showing a set
instance (Show a) => Show (Set a) where
  show (St xs)  = "ListSet(" ++ Data.List.intercalate "," (map show xs) ++ ")"

-- Set equality
instance (Eq a) => Eq (Set a) where
  s == s'  = s `isSubsetOf` s' && s' `isSubsetOf` s

-- Implements non strict set inclusion
isSubsetOf :: (Eq a) => Set a -> Set a -> Bool
(St xs) `isSubsetOf` (St ys) = isSub xs ys
 where
   []     `isSub` _   = True
   (x:xs) `isSub` ys  = x `elem` ys && xs `isSub` ys

-- This instance is used by QuickCheck to generate random sets
instance (Eq a, Arbitrary a) => Arbitrary (Set a) where
    arbitrary  = do
      xs <- listOf arbitrary
      return (foldr insert empty xs)
