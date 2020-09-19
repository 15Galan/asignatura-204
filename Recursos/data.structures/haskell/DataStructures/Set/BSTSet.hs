-------------------------------------------------------------------------------
-- Sets implemented by means of binary search trees
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module DataStructures.Set.BSTSet
  ( Set
  , empty
  , isEmpty
  , size
  , insert
  , isElem
  , notIsElem
  , delete

  , fold

  , union
  , intersection
  , difference
  ) where

import Data.List(intercalate)
import Test.QuickCheck
import qualified DataStructures.SearchTree.BST as T

data Set a  = S (T.BST a)

empty :: Set a
empty  = S T.empty

isEmpty :: Set a -> Bool
isEmpty (S tree)  = T.isEmpty tree

size :: Set a -> Int
size (S tree)  = T.size tree

insert :: (Ord a) => a -> Set a -> Set a
insert x (S tree)  = S (T.insert x tree) -- relies on BST not admiting repeated elements

isElem :: (Ord a) => a -> Set a -> Bool
isElem x (S tree)  = T.isElem x tree

notIsElem :: (Ord a) => a -> Set a -> Bool
notIsElem x s  = not (isElem x s)

delete :: (Ord a) => a -> Set a -> Set a
delete x (S tree)  = S (T.delete x tree)

fold :: (a -> b -> b) -> b -> Set a -> b
fold f z (S tree)  = T.foldInOrder f z tree

union :: (Ord a) => Set a -> Set a -> Set a
union s s'  = fold insert s s'

difference :: (Ord a) => Set a -> Set a -> Set a
difference s s'  = fold delete s s'

intersection :: (Ord a) => Set a -> Set a -> Set a
intersection s s' = fold (\x inter -> if isElem x s then insert x inter else inter) empty s'

elems :: Set a -> [a]
elems  = fold (:) []

-- Showing sets
instance (Show a) => Show (Set a) where
  show s  = "BSTSet(" ++ intercalate "," (map show $ elems s) ++ ")"

-- Set equality
instance (Eq a) => Eq (Set a) where
        s == s'  = elems s == elems s'

-- This instance is used by QuickCheck to generate random sets
instance (Arbitrary a, Ord a) => Arbitrary (Set a) where
    arbitrary  = do
      xs <- listOf arbitrary
      return (foldr insert empty xs)

