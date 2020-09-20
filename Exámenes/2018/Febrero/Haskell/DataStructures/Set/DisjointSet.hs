-------------------------------------------------------------------------------
-- Student's name:  Antonio J.
-- Student's group: Galán Herrera
--
-- Data Structures. February 2018. BSc. Computer Science. UMA.
-------------------------------------------------------------------------------

module DataStructures.Set.DisjointSet
                  ( DisjointSet
                  , empty
                  , isEmpty
                  , isElem
                  , numElements
                  , add
                  , areConnected
                  , kind
                  , union
                  , flatten
                  , kinds
                  ) where

import           Data.List                               (intercalate)
import           Data.Maybe                              (fromJust)
import qualified DataStructures.Dictionary.AVLDictionary as D

data DisjointSet a = DS (D.Dictionary a a)



-- | Exercise 1. empty
empty :: DisjointSet a
empty = DS (D.empty)


-- | Exercise 2.a isEmpty
isEmpty :: DisjointSet a -> Bool
isEmpty (DS d) = D.isEmpty d


-- | Exercise 2.b isElem
isElem :: (Ord a) => a -> DisjointSet a -> Bool
isElem e (DS d) = D.isDefinedAt e d


-- | Exercise 3. numElements
numElements :: DisjointSet a -> Int
numElements (DS d) = D.size d


-- | Exercise 4. add
add :: Ord a => a -> DisjointSet a -> DisjointSet a
add x ds@(DS d) | x `isElem` ds = ds
                | otherwise     = (DS d')
  where
    d' = D.insert x x d


-- | Exercise 5. root
root :: Ord a => a -> DisjointSet a -> Maybe a
root x ds@(DS d) | not $ x `isElem` ds = Nothing
                 | x == e              = Just x
                 | otherwise           = root e ds
  where
    e = fromJust(D.valueOf x d)


-- | Exercise 6. isRoot
isRoot :: Ord a => a -> DisjointSet a -> Bool
isRoot x d = root x d == Just x


-- | Exercise 7. areConnected
areConnected :: Ord a => a -> a -> DisjointSet a -> Bool
areConnected x y d = mismaraiz && pertenecen
  where
    mismaraiz = root x d == root x d
    pertenecen = x `isElem` d && y `isElem` d


-- | Exercise 8. kind
kind :: Ord a => a -> DisjointSet a -> [a]
kind x ds@(DS d) = [ e | e <- D.keys d , root e ds == root x ds ]


-- | Exercise 9. union
union :: Ord a => a -> a -> DisjointSet a -> DisjointSet a
union x y ds@(DS d) = DS (D.insert mayor menor d)
  where
    (mayor, menor) = if raizX >= raizY then (raizX, raizY) else (raizY, raizX)
    (Just raizX, Just raizY) = (root x ds, root y ds)

-- | ------------------------------------------------------------------------

flatten :: Ord a => DisjointSet a -> DisjointSet a
flatten ds@(DS d) = aux ds (D.keysValues d) D.empty
  where
    aux ds [] ds' = DS ds'
    aux ds ((a,b):xs) ds' = aux ds xs (D.insert a (fromJust(root a ds)) ds')

kinds :: Ord a => DisjointSet a -> [[a]]
kinds ds@(DS d) = [ kind e ds | e <- D.keys d , e `isRoot` ds ]

-- | ------------------------------------------------------------------------

instance (Ord a, Show a) => Show (DisjointSet a) where
  show (DS d)  = "DS(" ++ intercalate "," (map show (D.keysValues d)) ++ ")"


{-

-- Examples

-- | Exercise 1. empty

>>> empty
DictionaryDisjointSet()

-- | Exercise 2.a isEmpty

>>> isEmpty empty
True

>>> isEmpty (add 1 empty)
False

-- | Exercise 2.b isElem

>>> isElem 1 empty
False

>>> isElem 1 (add 1 empty)
True

>>> isElem 2 (add 1 empty)
False

>>> isElem 1 (add 2 (add 1 empty))
True

-- | Exercise 3. numElements

>>> numElements empty
0

>>> numElements (add 1 empty)
1

>>> numElements (add 2 (add 1 empty))
2

-- | Exercise 4. add

>>> add 1 empty
DictionaryDisjointSet((1,1))

>>> add 2 (add 1 empty)
DictionaryDisjointSet((1,1),(2,2))

>>> add 1 (add 2 (add 1 empty))
DictionaryDisjointSet((1,1),(2,2))

-- | Exercise 5. root

>>> root 1 empty
Nothing

>>> root 1 (add 1 empty)
Just 1

>>> root 2 (add 2 (add 1 empty))
Just 2

>>> root 1 (union 1 2 (add 2 (add 1 empty)))
Just 1

>>> root 2 (union 1 2 (add 2 (add 1 empty)))
Just 1

>>> root 1 (union 1 3 (add 3 (add 2 (add 1 empty))))
Just 1

>>> root 2 (union 1 3 (add 3 (add 2 (add 1 empty))))
Just 2

>>> root 3 (union 1 3 (add 3 (add 2 (add 1 empty))))
Just 1

>>> root 4 (union 1 3 (add 3 (add 2 (add 1 DisjointSet.empty))))
Nothing

-- | Exercise 6. isRoot

>>> isRoot 1 empty
False

>>> isRoot 1 (add 1 empty)
True

>>> isRoot 1 (union 1 2 (add 2 (add 1 empty)))
True

>>> isRoot 2 (union 1 2 (add 2 (add 1 empty)))
False

>>> isRoot 1 (union 1 3 (add 3 (add 2 (add 1 empty))))
True

>>> isRoot 2 (union 1 3 (add 3 (add 2 (add 1 empty))))
True

>>> isRoot 3 (union 1 3 (add 3 (add 2 (add 1 empty))))
False

-- | Exercise 7. areConnected

>>> areConnected 1 3 (union 1 3 (add 3 (add 2 (add 1 empty))))
True

>>> areConnected 3 1 (union 1 3 (add 3 (add 2 (add 1 empty))))
True

>>> areConnected 1 1 (union 1 3 (add 3 (add 2 (add 1 empty))))
True

>>> areConnected 1 2 (union 1 3 (add 3 (add 2 (add 1 empty))))
False

>>> areConnected 1 2 (union 2 3 (union 1 3 (add 3 (add 2 (add 1 empty)))))
True

>>> areConnected 1 5 (union 2 3 (union 1 3 (add 3 (add 2 (add 1 empty)))))
False

-- | Exercise 8. kind

>>> kind 1 (add 2 (add 1 empty))
[1]

>>> kind 2 (add 2 (add 1 empty))
[2]

>>> kind 3 (add 2 (add 1 empty))
[]

>>> kind 1 (union 1 3 (add 3 (add 2 (add 1 empty))))
[1,3]

>>> kind 3 (union 1 3 (add 3 (add 2 (add 1 empty))))
[1,3]

>>> kind 2 (union 1 3 (add 3 (add 2 (add 1  empty))))
[2]

>>> kind 2 (union 2 3 (union 1 3 (add 3 (add 2 (add 1 empty)))))
[1,2,3]

-- | Exercise 9. union

>>> union 1 2 (add 2 (add 1 empty))
DictionaryDisjointSet((1,1),(2,1))

>>> union 2 1 (add 2 (add 1 empty))
DictionaryDisjointSet((1,1),(2,1))

>>> union 1 1 (add 2 (add 1 empty))
DictionaryDisjointSet((1,1),(2,2))

>>> union 1 3 (add 3 (add 2 (add 1 empty)))
DictionaryDisjointSet((1,1),(2,2),(3,1))

>>> union 1 2 (add 1 empty)
*** Exception: union: missing element(s)

-}
