-------------------------------------------------------------------------------
-- Simple client module using a Priority Queue
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module Demos.PriorityQueue.PriorityQueueDemo where

--import DataStructures.Heap.WBLHPriorityQueue
import DataStructures.PriorityQueue.LinearPriorityQueue
import DataStructures.PriorityQueue.PriorityQueueAxioms(priorityQueueAxioms)


q1 :: PQueue Int
q1 = enqueue 2 (enqueue 3 (enqueue 1 empty))

size :: (Ord a) => PQueue a -> Int
size s
  | isEmpty s =  0
  | otherwise =  1 + size (dequeue s)


