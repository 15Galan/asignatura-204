-------------------------------------------------------------------------------
-- Simple client module using a Queue
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module Demos.Queue.QueueDemo where

import DataStructures.Queue.LinearQueue
import DataStructures.Queue.QueueAxioms(queueAxioms)


q1 :: Queue Int
q1 = enqueue 3 (enqueue 2 (enqueue 1 empty))

size :: Queue a -> Int
size s
  | isEmpty s =  0
  | otherwise =  1 + size (dequeue s)
