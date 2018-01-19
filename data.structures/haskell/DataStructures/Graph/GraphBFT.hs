-------------------------------------------------------------------------------
-- Breadth First Traversal of a Graph
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module DataStructures.Graph.GraphBFT
  ( bft
  , bftPaths
  ) where

import DataStructures.Graph.Graph
import DataStructures.Queue.TwoListsQueue
import qualified DataStructures.Set.BSTSet as S
import qualified DataStructures.Dictionary.BSTDictionary as D
import DataStructures.Graph.Utils


bft :: (Ord a) => Graph a -> a -> [a]
bft g v0  = aux S.empty (enqueue v0 empty)
 where
  aux visited queue
   | isEmpty queue         = [] -- end of traversal
   | v `S.isElem` visited  = aux visited queue' -- v was already visited
   | otherwise             = v : aux visited' (enqueueAll queue' us)
   where
     v  = first queue
     queue'  = dequeue queue
     visited'  = S.insert v visited
     us  = [ u | u <- successors g v, u `S.notIsElem` visited ]

data DiEdge a  = a :-> a deriving Show

-- Returns paths from source to all vertices that are
-- visited during breadth first traversal
bftPaths :: (Ord a) => Graph a -> a -> [Path a]
bftPaths g v0  = aux S.empty (enqueue (v0 :-> v0) empty) D.empty
 where
  aux visited queue dict
   | isEmpty queue         = [] -- end of traversal
   | v `S.isElem` visited  = aux visited queue' dict -- v is already visited
   | otherwise             =
       pathFromTo v0 v dict' : -- v has been visited. Return path to it
         aux visited' (enqueueAll queue' es) dict'
   where
     w :-> v  = first queue
     queue'  = dequeue queue
     visited'  = S.insert v visited
     dict'  = D.insert v w dict -- parent of v is w
     es  = [ v :-> u | u <- successors g v, u `S.notIsElem` visited ]

