import TwoListsDoubleEndedQueue
import DoubleEndedQueueAxioms

q1 = foldOp empty [addFirst 1, addLast 2, addFirst 3]
  where
      foldOp z []     = z
      foldOp z (o:os) = foldOp (o z) os

-- Main> q1
-- TwoListsDoubleEndedQueue(3,1,2)

main = dqAxioms
