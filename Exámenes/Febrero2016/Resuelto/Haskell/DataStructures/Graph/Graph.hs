-------------------------------------------------------------------------------
-- Graph defined by list of vertices and adjancency function
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module DataStructures.Graph.Graph
  ( Graph
  , Edge
  , Path
  , empty
  , mkGraphEdges
  , mkGraphSuc
  , isEmpty
  , successors
  , edges
  , vertices
  , degree
  , addVertex
  , addEdge
  , deleteVertex
  , deleteEdge
  ) where

import Data.List(nub, nubBy, delete, deleteBy, intercalate)

type Edge a = (a,a)

cmpEdges :: (Eq a) => Edge a -> Edge a -> Bool
cmpEdges t@(x,y) t' = t==t' || (y,x) == t'

type Path a = [a] -- Path represented as list of vertices

data Graph a = G [a] (a -> [a])
-- invariants for G vs sucs:
-- vs has no repetitions
-- sucs v has no repetitions

empty :: Graph a
empty = G [] (\_ -> [])

isEmpty :: Graph a -> Bool
isEmpty (G vs _) = null vs

mkGraphSuc :: (Eq a) => [a] -> (a -> [a]) -> Graph a
mkGraphSuc vs sucs = G (nub vs) sucs'
 where
   sucs' v = nub (sucs v) -- enforces no repetitions in successors

mkGraphEdges :: (Eq a) => [a] -> [Edge a] -> Graph a
mkGraphEdges vs es = G (nub vs) sucs
 where
   sucs v = nub $ [ y | (x,y) <- es, x==v ] -- enforces no repetitions
                  ++
                  [ x | (x,y) <- es, y==v ]

successors :: (Eq a) => Graph a -> a -> [a]
successors (G vs sucs) v = sucs' v
 where
   sucs' v
    | elem v vs = sucs v
    | otherwise = []    -- this is consistent with Java definition
                        -- only problem is that if function provided
                        -- to mkGraphSuc is partial, successors may fail

vertices :: Graph a -> [a]
vertices (G vs sucs) = vs

edges :: (Eq a) => Graph a -> [Edge a]
edges (G vs sucs) = nubBy cmpEdges [ (v,w) | v <- vs, w <- sucs v ]

degree :: (Eq a) => Graph a -> a -> Int
degree g v = length (successors g v)

deleteEdge :: (Eq a) => Graph a -> Edge a -> Graph a
deleteEdge g (v,w) = mkGraphEdges (vertices g) (deleteBy cmpEdges (v,w) (edges g))

deleteVertex :: (Eq a) => Graph a -> a -> Graph a
deleteVertex (G vs sucs) v = mkGraphSuc (delete v vs) sucs'
      where sucs' v = delete v (sucs v)

addVertex :: (Eq a) => Graph a -> a -> Graph a
addVertex (G vs sucs) v = G (nub (v:vs)) sucs

addEdge :: (Eq a) => Graph a -> Edge a -> Graph a
addEdge g e@(v,w)
  | any (`notElem` vs) [v,w] = error "addEdge: vertices not in graph"
  | otherwise                = mkGraphEdges vs (e : edges g)
  where
    vs = vertices g

instance (Eq a, Show a) => Show (Graph a) where
  show g@(G vs sucs)  = "Graph("++vertices++", "++arcs++")"
   where
    vertices  = "["++ intercalate "," (map show vs) ++"]"
    arcs  = "[" ++ intercalate ", " (map showEd $ edges g) ++ "]"
    showEd (x,y)  = "("++show x++","++show y++")"
