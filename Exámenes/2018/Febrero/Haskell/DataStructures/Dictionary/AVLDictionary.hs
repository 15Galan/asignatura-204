-------------------------------------------------------------------------------
-- Dictionaries implemented by using AVL Trees
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module DataStructures.Dictionary.AVLDictionary
  ( Dictionary
  , empty
  , isEmpty
  , size
  , insert
  , updateOrInsert
  , valueOf
  , isDefinedAt
  , delete

  , keys
  , values
  , keysValues

  , foldKeys
  , foldValues
  , foldKeysValues
  ) where

import Data.Function(on)
import Data.List(intercalate)
import Data.Maybe(isJust)
import Test.QuickCheck
import qualified DataStructures.SearchTree.AVL as T

data Rel a b  = a :-> b

key :: Rel a b -> a
key (k :-> _)  = k

value :: Rel a b -> b
value (_ :-> v)  = v

withKey :: a -> Rel a b
withKey k  = k :-> undefined

-- Relations are compared by using only their keys
instance (Eq a) => Eq (Rel a b) where
  (==)  = (==) `on` key

instance (Ord a) => Ord (Rel a b) where
  compare  = compare `on` key

newtype Dictionary a b  = D (T.AVL (Rel a b))

empty :: Dictionary a b
empty  = D T.empty

isEmpty :: Dictionary a b -> Bool
isEmpty (D avl)  = T.isEmpty avl

size :: Dictionary a b -> Int
size (D avl)  = T.size avl

insert :: (Ord a) => a -> b -> Dictionary a b -> Dictionary a b
insert k v (D avl)  = D (T.insert (k :-> v) avl)

updateOrInsert :: (Ord a) => a -> (b -> b) -> b -> Dictionary a b -> Dictionary a b
updateOrInsert k f v (D avl)  = D (T.updateOrInsert  f' (k :-> v) avl)
 where f' (k :-> v)  = k :-> f v

valueOf :: (Ord a) => a -> Dictionary a b -> Maybe b
valueOf k (D avl)  =
  case T.search (withKey k) avl of
    Nothing         -> Nothing
    Just (_ :-> v') -> Just v'

isDefinedAt :: (Ord a) => a -> Dictionary a b -> Bool
isDefinedAt k d  = isJust (valueOf k d)

delete :: (Ord a) => a -> Dictionary a b -> Dictionary a b
delete k (D avl)  = D (T.delete (withKey k) avl)

keys :: Dictionary a b -> [a]
keys (D avl)  = map key (T.inOrder avl)

values :: Dictionary a b -> [b]
values (D avl)  = map value (T.inOrder avl)

keysValues :: Dictionary a b -> [(a,b)]
keysValues (D avl)  = map toTuple (T.inOrder avl)
 where toTuple (k :-> v) = (k,v)

foldKeys :: (a -> c -> c) -> c -> Dictionary a b -> c
foldKeys f z (D avl)  = T.foldInOrder (f . key) z avl

foldValues :: (b -> c -> c) -> c -> Dictionary a b -> c
foldValues f z (D avl)  = T.foldInOrder (f . value) z avl

foldKeysValues :: (a -> b -> c -> c) -> c -> Dictionary a b -> c
foldKeysValues f z (D avl)  = T.foldInOrder (\(k :-> v) -> f k v) z avl

instance (Show a, Show b) => Show (Dictionary a b) where
  show (D avl)  = "AVLDictionary(" ++ intercalate "," (aux (T.inOrder avl)) ++ ")"
   where
    aux []             = []
    aux (x:->y : xys)  = (show x++"->"++show y) : aux xys

instance (Ord a, Arbitrary a, Arbitrary b) => Arbitrary (Dictionary a b) where
    arbitrary  = do
      kvs <- listOf arbitrary
      return (foldr (\(k,v) -> insert k v) empty kvs)

instance (Eq a, Eq b) => Eq (Dictionary a b) where
  d == d' = keysValues d == keysValues d'
