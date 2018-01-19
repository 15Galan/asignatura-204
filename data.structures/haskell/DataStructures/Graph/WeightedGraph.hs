-------------------------------------------------------------------------------
-- Weighted Graph defined by list of vertices and adjancency function
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module DataStructures.Graph.WeightedGraph
  ( WeightedGraph
  , WeightedEdge(WE)
  , Path
  , mkWeightedGraphEdges
  , mkWeightedGraphAdj
  , vertices
  , edges
  , weigthedEdges
  , successors
  , degree
  ) where

import Data.List(nub, intercalate, nubBy)

data WeightedEdge a w  = WE a w a deriving Show

type Path a  = [a] -- Path represented as list of vertices

data WeightedGraph a w  = WG [a] (a -> [(a,w)])

mkWeightedGraphAdj :: (Eq a) => [a] -> (a -> [(a,w)]) -> WeightedGraph a w
mkWeightedGraphAdj vs sucs  = WG (nub vs) sucs

mkWeightedGraphEdges :: (Eq a, Eq w) => [a] -> [WeightedEdge a w] -> WeightedGraph a w
mkWeightedGraphEdges vs es  = WG (nub vs) sucs
 where
   sucs v  = nub [ (y,w) | WE x w y <- es, x==v ]
                 ++
                 [ (x,w) | WE x w y <- es, y==v ]

successors :: WeightedGraph a w -> a -> [(a,w)]
successors (WG vs sucs) v  = sucs v

vertices :: WeightedGraph a w -> [a]
vertices (WG vs sucs)  = vs

edges :: (Eq a) => WeightedGraph a w -> [WeightedEdge a w]
edges (WG vs sucs)  = [ WE v w u | v <- vs, (u,w) <- sucs v ]

weigthedEdges :: WeightedGraph a w -> [WeightedEdge a w]
weigthedEdges (WG vs sucs)  = [ WE v w u | v <- vs, (u,w) <- sucs v ]

degree :: WeightedGraph a w -> a -> Int
degree g v  = length (successors g v)


instance (Eq a, Show a, Show w) => Show (WeightedGraph a w) where
  show g@(WG vs sucs)  = "WeightedGraph("++vertices++","++arcs++")"
   where
    vertices  = "("++ intercalate "," (map show vs) ++")"
    arcs  = "(" ++ intercalate ", " (map showEd $ nubBy cmp (edges g)) ++ ")"
    cmp (WE x _ y) (WE x' _ y')  = (x==x' && y==y') || (x==y' && y==x')
    showEd (WE x w y)  = intercalate "-" [ show x, show w, show y ]
