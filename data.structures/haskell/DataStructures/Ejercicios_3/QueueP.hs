module QueueP (
    QueueP
    , isEmpty
    , enqueue
    , dequeue
    , first
    , empty
    ) where

import Data.List(intercalate)
import Test.QuickCheck

data QueueP a = Empty | Node a (QueueP a)


isEmpty :: QueueP a -> Bool
isEmpty Empty = True
isEmpty _     = False

enqueue :: (Ord a) => a -> QueueP a -> QueueP a
enqueue x Empty      = Node x Empty                     -- Como el nodo debe introducirse de forma
enqueue x (Node y q) | x <= y = Node x (Node y q)       -- ordenada, este esta es la única función
                     | x > y  = Node y (enqueue x q)    -- que verdaderamente cambia.

dequeue :: QueueP a -> QueueP a
dequeue Empty      = error "dequeue on empty priority-queue"
dequeue (Node x q) = q

first :: QueueP a -> a
first Empty      = error "first on empty priority-queue"
first (Node x q) = x

empty :: QueueP a
empty =  Empty

-- Showing a priority-queue
instance (Show a) => Show (QueueP a) where
  show q = "PriorityQueue(" ++ intercalate "," (aux q) ++ ")"
    where
    aux Empty      = []
    aux (Node x q) = show x : aux q

-- Priority-queue equality
instance (Eq a) => Eq (QueueP a) where
    Empty      == Empty        =  True
    (Node x q) == (Node x' q') =  x==x' && q==q'
    _          == _            =  False

-- This instance is used by QuickCheck to generate random queues
instance (Arbitrary a, Ord a) => Arbitrary (QueueP a) where
    arbitrary = do
      xs <- listOf arbitrary
      return (foldr enqueue empty xs)

