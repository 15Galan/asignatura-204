-------------------------------------------------------------------------------
-- Breadth First Traversal of a weighted directed graph.
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module DataStructures.Graph.WeightedDiGraphBFT
  ( bft
  , bftPaths
  , bftPathTo
  ) where

import DataStructures.Graph.WeightedDiGraph
import DataStructures.Graph.Utils(enqueueAll)
import DataStructures.Queue.TwoListsQueue
import qualified DataStructures.Set.BSTSet as S
import qualified DataStructures.Dictionary.BSTDictionary as D

bft :: (Ord a) => WeightedDiGraph a w -> a -> [a] 
bft g v0 = aux S.empty (enqueue v0 empty)
 where
  aux visited queue
   | isEmpty queue      = [] -- end of traversal
   | v `S.isElem` visited = aux visited queue' -- v was already visited
   | otherwise            = v : aux visited' (enqueueAll queue' us)  
   where
     v = first queue
     queue' = dequeue queue
     visited' = S.insert v visited
     us = [ u | (u,w) <- successors g v, u `S.notIsElem` visited ]


-- Returns paths from source to all vertices that are 
-- visited during breadth first traversal

bftPaths :: (Ord a) => WeightedDiGraph a w -> a -> [Path (WDiEdge a w)] 
bftPaths g v0 = tail (aux S.empty (enqueue (E v0 undefined v0) empty) D.empty)
 where
  aux visited queue dict
   | isEmpty queue        = [] -- end of traversal
   | v `S.isElem` visited = aux visited queue' dict -- v is already visited
   | otherwise            = 
       pathFromTo v0 v dict' : -- v has been visited. Return path to it
         aux visited' (enqueueAll queue' es) dict' 
   where
     (E u w v) = first queue
     queue' = dequeue queue
     visited' = S.insert v visited
     dict' = D.insert v (u,w) dict -- parent of v is u with weight w
     es = [ E v w u | (u,w) <- successors g v, u `S.notIsElem` visited ]

bftPathTo :: (Ord a) => WeightedDiGraph a w -> a -> a -> Maybe (Path (WDiEdge a w))
bftPathTo g x y =  aux (filter ((\(E _ _ v) -> v == y) . last) (bftPaths g x))
  where
    aux []     = Nothing
    aux [path] = Just path

pathFromTo :: (Ord a) => a -> a -> D.Dictionary a (a,w) -> Path (WDiEdge a w)
pathFromTo v0 v src = reverse (aux v)
 where
  aux v
   | v == v0   = [] 
   | otherwise = (E u w v) : aux u 
   where Just (u,w) = D.valueOf v src
