-------------------------------------------------------------------------------
-- Topological Sorting of a Graph
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module DataStructures.Graph.TopologicalSorting
  ( topSorting
  , topSortings
  ) where

import DataStructures.Graph.DiGraph
import Data.List((\\))

-- returns vertices in vs with no predecesor (in vs)
noPreds :: (Eq a) => DiGraph a -> [a] -> [a]
noPreds g vs  = [ v | v <- vs,  null (preds v) ]
 where
   -- returns predecesors (in vs) of v
   preds v  = [ w | w <- vs, v `elem` successors g w ]


topSorting :: (Ord a) => DiGraph a -> [a]
topSorting g  = aux (vertices g)
 where
  aux [] = []
  aux vs
   | null ws    = error "DiGraph is cyclic"
   | otherwise  = v : aux (vs \\ [v])
   where
     ws = noPreds g vs
     v = head ws


topSortings :: (Ord a) => DiGraph a -> [[a]]
topSortings g  = aux (vertices g)
 where
  aux []        = [ [] ]
  aux vs
   | null ws    = error "DiGraph is cyclic"
   | otherwise  = [ v : ts
                  | v <- noPreds g vs
                  , ts <- aux (vs \\ [v])
                  ]
   where
     ws  = noPreds g vs
     v  = head ws
