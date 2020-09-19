-------------------------------------------------------------------------------
-- Axioms for a Queue
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module DataStructures.Queue.QueueAxioms(ax1,ax2,ax3,ax4,ax5,ax6,queueAxioms) where

import DataStructures.Queue.LinearQueue -- TwoStacksQueue  -- TwoListsQueue
import Test.QuickCheck

-- an element enqueueed on an empty queue is placed at the first of the queue
ax1 x   =  True ==>  first (enqueue x empty) == x

-- enqueueing an element on a non-empty queue does not change its first element
ax2 x q =  not (isEmpty q) ==>  first q == first (enqueue x q)

-- for non-empty queues, dequeue and enqueue commute
ax3 x q =  not (isEmpty q) ==>  dequeue (enqueue x q) == enqueue x (dequeue q)

-- enqueueing and removing an element on an empty queue returns an empty queue
ax4 x   =  True ==>  dequeue (enqueue x empty) == empty

-- the empty queue is empty
ax5     =  True ==>  isEmpty empty

-- enqueue always returns non-empty queues
ax6 x q =  True ==>  not (isEmpty (enqueue x q))


type Elem = Int

queueAxioms = do
  quickCheck (ax1 :: Elem -> Property)
  quickCheck (ax2 :: Elem -> Queue Elem -> Property)
  quickCheck (ax3 :: Elem -> Queue Elem -> Property)
  quickCheck (ax4 :: Elem -> Property)
  quickCheck (ax5 :: Property)
  quickCheck (ax6 :: Elem -> Queue Elem -> Property)
