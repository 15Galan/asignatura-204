-------------------------------------------------------------------------------
-- Graph defined by list of vertices and adjancency function
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2011
-------------------------------------------------------------------------------

module Graph   
  ( Graph -- data Graph a = ...
  , Edge  -- type Edge a = (a,a)
  , Path --  type Path a = [a] 
  , mkGraphSuc    -- :: [a] -> (a -> [a]) -> Graph a
  , mkGraphEdges  -- :: (Eq a) => [a] -> [Edge a] -> Graph a
  , successors     -- :: Graph a -> a -> [a]
  , vertices      -- :: Graph a -> [a]
  , edges         -- :: (Eq a) => Graph a -> [Edge a]
  , degree        -- :: Graph a -> a -> Int
  , (\\\)         -- :: Graph a -> Graph a 
  ) where

import List(nub,(\\))

type Edge a = (a,a)

type Path a = [a] -- Path represented as list of vertices

data Graph a = G [a] (a -> [a])

mkGraphSuc :: [a] -> (a -> [a]) -> Graph a
mkGraphSuc vs suc = G vs suc

mkGraphEdges :: (Eq a) => [a] -> [Edge a] -> Graph a
mkGraphEdges vs es = G vs suc
 where
   suc v = nub $ [ y | (x,y) <- es, x==v] 
                 ++ 
                 [ x | (x,y) <- es, y==v]                

successors :: Graph a -> a -> [a]
successors (G vs suc) v = suc v 

vertices :: Graph a -> [a]
vertices (G vs suc) = vs

edges :: (Eq a) => Graph a -> [Edge a]
edges (G vs suc) = nub $ concat [ [(v,w), (w,v)] | v <- vs, w <- suc v ] 

degree :: Graph a -> a -> Int
degree g v = length (successors g v)

(\\\)  :: Eq a => Graph a -> [a] -> Graph a

(G vs suc) \\\ xs = G (vs\\xs) suc' 
   where suc' v = suc v \\ xs

instance Show (Graph a) where
  show g = "Graph"