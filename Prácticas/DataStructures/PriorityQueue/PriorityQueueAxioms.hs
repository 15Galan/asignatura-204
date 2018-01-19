-------------------------------------------------------------------------------
-- Axioms for Priority Queues
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module DataStructures.PriorityQueue.PriorityQueueAxioms(ax1,ax2,ax3,ax4,ax5,ax6,priorityQueueAxioms) where

import DataStructures.PriorityQueue.LinearPriorityQueue
-- import DataStructures.PriorityQueue.WBLeftistHeapPriorityQueue
-- import DataStructures.PriorityQueue.MaxiphobicHeapPriorityQueue
import Test.QuickCheck

ax1        = True    ==>  isEmpty empty
ax2 x q    = True    ==>  not (isEmpty (enqueue x q))
ax3 x      = True    ==>  first (enqueue x empty) == x
ax4 x y q  = True    ==>  first (enqueue y (enqueue x q)) ==
                            first (enqueue (min x y) q)
ax5 x      = True    ==>  dequeue (enqueue x empty) == empty
ax6 x y q  = True    ==>  dequeue (enqueue y (enqueue x q)) ==
                            enqueue (max x y) (dequeue (enqueue (min x y) q))

type Elem = Int

priorityQueueAxioms = do
  quickCheck (ax1 :: Property)
  quickCheck (ax2 :: Elem -> PQueue Elem  -> Property)
  quickCheck (ax3 :: Elem ->Property)
  quickCheck (ax4 :: Elem -> Elem -> PQueue Elem -> Property)
  quickCheck (ax5 :: Elem ->Property)
  quickCheck (ax6 :: Elem -> Elem -> PQueue Elem -> Property)
