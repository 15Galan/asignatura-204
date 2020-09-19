-------------------------------------------------------------------------------
-- Weighted DiGraph defined by list of vertices and succesors function
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module DataStructures.Graph.WeightedDiGraph
  ( WeightedDiGraph
  , WeightedDiEdge(E)
  , Path
  , mkWeightedDiGraphEdges
  , mkWeightedDiGraphSuc
  , vertices
  , weightedDiEdges
  , successors
  , predecesors
  , inDegree
  , outDegree
  ) where

import Data.List(nub, intercalate)

data WeightedDiEdge a w  = E a w a deriving (Eq,Show)

type Path a  = [a] -- Path represented as list of vertices

data WeightedDiGraph a w  = WDG [a] (a -> [(a,w)])

mkWeightedDiGraphSuc :: (Eq a) => [a] -> (a -> [(a,w)]) -> WeightedDiGraph a w
mkWeightedDiGraphSuc vs sucs  = WDG (nub vs) sucs

mkWeightedDiGraphEdges :: (Eq a) => [a] -> [WeightedDiEdge a w] -> WeightedDiGraph a w
mkWeightedDiGraphEdges vs es  = WDG (nub vs) sucs
 where
   sucs v  = [ (y,w) | E x w y <- es, x==v ]

successors :: WeightedDiGraph a w -> a -> [(a,w)]
successors (WDG vs sucs) v  = sucs v

predecesors :: (Eq a) => WeightedDiGraph a w -> a -> [(a,w)]
predecesors (WDG vs sucs) u  = [ (v,w) | v <- vs, w <- weightFor u (sucs v) ]
 where
   weightFor v []  = []
   weightFor v ((u,w):wus)
     | v==u        = [w]
     | otherwise   = weightFor v wus

vertices :: WeightedDiGraph a w -> [a]
vertices (WDG vs sucs)  = vs

weightedDiEdges :: WeightedDiGraph a w -> [WeightedDiEdge a w]
weightedDiEdges (WDG vs sucs)  = [ E v w u | v <- vs, (u,w) <- sucs v ]

outDegree :: WeightedDiGraph a w -> a -> Int
outDegree g v  = length (successors g v)

inDegree :: (Eq a) => WeightedDiGraph a w -> a -> Int
inDegree g v  = length (predecesors g v)

instance (Eq a, Eq w, Show a, Show w) => Show (WeightedDiGraph a w) where
  show g@(WDG vs sucs)  = "WeightedDiGraph("++vertices++","++arcs++")"
   where
    vertices  = "("++ intercalate "," (map show vs) ++")"
    arcs  = "(" ++ intercalate ", " (map showDiEd (weightedDiEdges g)) ++ ")"
    showDiEd (E x w y)  = intercalate "->" [show x, show w, show y]