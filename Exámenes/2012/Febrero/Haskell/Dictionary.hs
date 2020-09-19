-------------------------------------------------------------------------------
-- Dictionaries implemented by using Binary Search Trees
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2011
-------------------------------------------------------------------------------

module Dictionary
  ( Dictionary
  , empty
  , isEmpty
  , insert
  , valueOf
  , valueOf'
  , isDefinedAt
  , delete
  , keys
  , values
  ) where

import Data.Function(on)
import Data.List(intercalate)
import Data.Maybe(isJust)
import qualified BinarySearchTree as T

data Rel a b = a :-> b

key :: Rel a b -> a
key (k :-> v) = k

value :: Rel a b -> b
value (k :-> v) = v

withKey :: a -> Rel a b
withKey k = k :-> undefined

instance (Eq a) => Eq (Rel a b) where
  (==) = (==) `on` key

instance (Ord a) => Ord (Rel a b) where
  compare = compare `on` key

newtype Dictionary a b = M (T.BST (Rel a b)) 

empty :: Dictionary a b
empty = M T.empty

isEmpty :: Dictionary a b -> Bool
isEmpty (M avl) = T.isEmpty avl

insert :: (Ord a) => a -> b -> Dictionary a b -> Dictionary a b
insert k v (M avl) = M (T.insert (k :-> v) avl)

valueOf :: (Ord a) => a -> Dictionary a b -> Maybe b
valueOf k (M avl) = 
  case T.search (withKey k) avl of
    Nothing          -> Nothing
    Just (k' :-> v') -> Just v'

valueOf' :: (Ord a) => a -> Dictionary a b -> b
valueOf' k (M avl) = 
  case T.search (withKey k) avl of
    Just (k' :-> v') -> v'

isDefinedAt :: (Ord a) => a -> Dictionary a b -> Bool
isDefinedAt k map = isJust (valueOf k map) 

delete :: (Ord a) => a -> Dictionary a b -> Dictionary a b
delete k (M avl) = M (T.delete (withKey k) avl)

keys :: (Ord a) => Dictionary a b -> [a]
keys (M avl) = map key (T.inOrder avl)

values :: (Ord a) => Dictionary a b -> [b]
values (M avl) = map value (T.inOrder avl)

instance (Show a, Show b) => Show (Dictionary a b) where
  show (M avl) = "Dictionary(" ++ intercalate "," (aux (T.inOrder avl)) ++ ")"
   where
    aux []            = []
    aux (x:->y : xys) = (show x++"->"++show y) : aux xys