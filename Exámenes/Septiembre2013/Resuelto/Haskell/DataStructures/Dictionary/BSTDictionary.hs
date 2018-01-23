-------------------------------------------------------------------------------
-- Dictionaries implemented by using Binary Search Trees
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module DataStructures.Dictionary.BSTDictionary
  ( Dictionary
  , empty
  , isEmpty
  , insert
  , updateOrInsert
  , valueOf
  , isDefinedAt
  , delete
  , keys
  , values
  , keysAndValues
  ) where

import Data.Function(on)
import Data.List(intercalate)
import Data.Maybe(isJust)
import qualified DataStructures.SearchTree.BST as T

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

newtype Dictionary a b = D (T.BST (Rel a b)) 

empty :: Dictionary a b
empty = D T.empty

isEmpty :: Dictionary a b -> Bool
isEmpty (D bst) = T.isEmpty bst

insert :: (Ord a) => a -> b -> Dictionary a b -> Dictionary a b
insert k v (D bst) = D (T.insert (k :-> v) bst)

updateOrInsert :: (Ord a) => a -> (b -> b) -> b -> Dictionary a b -> Dictionary a b
updateOrInsert k f v (D bst) = D (T.updateOrInsert  f' (k :-> v) bst)
 where f' (k :-> v) = k :-> f v

valueOf :: (Ord a) => a -> Dictionary a b -> Maybe b
valueOf k (D bst) = 
  case T.search (withKey k) bst of
    Nothing          -> Nothing
    Just (k' :-> v') -> Just v'

isDefinedAt :: (Ord a) => a -> Dictionary a b -> Bool
isDefinedAt k map = isJust (valueOf k map) 

delete :: (Ord a) => a -> Dictionary a b -> Dictionary a b
delete k (D bst) = D (T.delete (withKey k) bst)

keys :: Dictionary a b -> [a]
keys (D bst) = map key (T.inOrder bst)

values :: Dictionary a b -> [b]
values (D bst) = map value (T.inOrder bst)

keysAndValues :: Dictionary a b -> [(a,b)] 
keysAndValues (D bst) = map toTuple (T.inOrder bst)
 where toTuple (k :-> v) = (k,v)


instance (Show a, Show b) => Show (Dictionary a b) where
  show (D bst) = "BSTDictionary(" ++ intercalate "," (aux (T.inOrder bst)) ++ ")"
   where
    aux []            = []
    aux (x:->y : xys) = (show x++"->"++show y) : aux xys