-------------------------------------------------------------------------------
-- Priority First Traversal
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module DataStructures.Graph.GraphPFT
  ( pftPaths
  ) where

import DataStructures.Graph.Graph
import DataStructures.PriorityQueue.WBLeftistHeapPriorityQueue
import qualified DataStructures.Set.BSTSet as S
import qualified DataStructures.Dictionary.BSTDictionary as D
import DataStructures.Graph.Utils(pathFromTo)

data DiEdge a = a :-> a deriving (Eq,Show)

-- Sort elements in priority queue according to destination
instance (Ord a) => Ord (DiEdge a) where
  (x :-> y) <= (x' :-> y') = y <= y'

-- Returns paths from source to all vertices that are 
-- visited during priority first traversal
pftPaths :: (Ord a) => Graph a -> a -> [Path a] 
pftPaths g v0 = aux S.empty (enqueue (v0 :-> v0) empty) D.empty 
 where
  aux visited pQueue dict
   | isEmpty pQueue       = [] -- end of traversal
   | v `S.isElem` visited = aux visited pQueue' dict -- v is already visited
   | otherwise            = 
       pathFromTo v0 v dict' : -- v has been visited. Return path to it
         aux visited' (enqueueAll pQueue' es) dict' 
   where
     w :-> v = first pQueue
     pQueue' = dequeue pQueue
     visited' = S.insert v visited
     dict' = D.insert v w dict -- parent of v is w
     es = [ v :-> u | u <- successors g v, u `S.notIsElem` visited ]

enqueueAll :: (Ord a) => PQueue a -> [a] -> PQueue a
enqueueAll s xs = foldr enqueue s xs

