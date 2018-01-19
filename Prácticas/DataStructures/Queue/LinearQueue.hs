-------------------------------------------------------------------------------
-- Inefficient Queue (enqueue is O(n))
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module DataStructures.Queue.LinearQueue
  ( Queue
  , empty
  , isEmpty
  , enqueue
  , first
  , dequeue
  ) where

import Data.List(intercalate)
import Test.QuickCheck

data Queue a =  Empty | Node a (Queue a)

empty :: Queue a
empty =  Empty

isEmpty :: Queue a -> Bool
isEmpty Empty =  True
isEmpty _     =  False

enqueue :: a -> Queue a -> Queue a
enqueue x Empty      =  Node x Empty
enqueue x (Node y q) =  Node y (enqueue x q)

first :: Queue a -> a
first Empty      =  error "first on empty queue"
first (Node x q) =  x

dequeue :: Queue a -> Queue a
dequeue Empty      =  error "dequeue on empty queue"
dequeue (Node x q) =  q

-- Showing a queue
instance (Show a) => Show (Queue a) where
  show q = "LinearQueue(" ++ intercalate "," (aux q) ++ ")"
    where
    aux Empty      =  []
    aux (Node x q) =  show x : aux q

-- Queue equality
instance (Eq a) => Eq (Queue a) where
    Empty      == Empty        =  True
    (Node x q) == (Node x' q') =  x==x' && q==q'
    _          == _            =  False

-- This instance is used by QuickCheck to generate random queues
instance (Arbitrary a) => Arbitrary (Queue a) where
    arbitrary = do
      xs <- listOf arbitrary
      return (foldr enqueue empty xs)

