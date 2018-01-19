-------------------------------------------------------------------------------
-- Linear implementation of Priority Queues
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-- Blas Ruiz, 2012, enqueue uses FIFO policy for elements with same priority
-------------------------------------------------------------------------------

module DataStructures.PriorityQueue.LinearPriorityQueue
  ( PQueue
  , empty
  , isEmpty
  , first
  , dequeue
  , enqueue
  ) where

import Data.List(intercalate)
import Test.QuickCheck

data PQueue a = Empty | Node a (PQueue a)

empty ::PQueue a
empty  = Empty

isEmpty ::PQueue a -> Bool
isEmpty Empty  = True
isEmpty _      = False

enqueue :: (Ord a) => a ->PQueue a -> PQueue a
enqueue x Empty  = Node x Empty
enqueue x (Node y q)
 | x < y         = Node x (Node y q)
 | otherwise     = Node y (enqueue x q)

first :: PQueue a -> a
first Empty       = error "first on empty priority queue"
first (Node x _)  = x

dequeue :: PQueue a -> PQueue a
dequeue Empty       = error "dequeue on empty priority queue"
dequeue (Node _ q)  = q

mkPQueue :: (Ord a) => [a] -> PQueue a
mkPQueue xs = foldr enqueue empty xs

-- Showing a priority PQueue
instance (Show a) => Show (PQueue a) where
  show q = "LinearPQueue(" ++ intercalate "," (aux q) ++ ")"
    where
     aux Empty      =  []
     aux (Node x q) =  show x : aux q

-- Priority Queue equality
instance (Eq a) => Eq (PQueue a) where
  Empty      == Empty           =  True
  (Node x q) == (Node x' q')    =  x==x' && q==q'
  _          == _               =  False

-- This instance is used by QuickCheck to generate random Priority Queues
instance (Ord a, Arbitrary a) => Arbitrary (PQueue a) where
    arbitrary =  do
      xs <- listOf arbitrary
      return (foldr enqueue empty xs)


