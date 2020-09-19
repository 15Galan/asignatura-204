-------------------------------------------------------------------------------
-- Ford-Fulkerson Algorithm. Maximal flow for a weighted directed graph.
--
-- Student's name:
-- Student's group:
--
-- Data Structures. Grado en Informática. UMA.
-------------------------------------------------------------------------------

module DataStructures.Graph.FordFulkerson where

import Data.List  ((\\))
import DataStructures.Graph.WeightedDiGraph
import DataStructures.Graph.WeightedDiGraphBFT



maxFlowPath :: Path (WDiEdge a Integer) -> Integer
maxFlowPath [] = 0
maxFlowPath path = minimum [ w | (E f w t) <- path ]


updateEdge ::(Eq a) => a -> a -> Integer -> [WDiEdge a Integer] -> [WDiEdge a Integer]
updateEdge x y p []  = [(E x p y)]
updateEdge x y p ((E f w t):es) | pertenece && sumacero     = es
                                | pertenece && not sumacero = (E f (w+p) t) : es
                                | otherwise                 = (E f w t) : updateEdge x y p es
  where
    pertenece = x == f && y == t
    sumacero  = (p+w) == 0


updateEdges :: (Eq a) => Path (WDiEdge a Integer) -> Integer -> [WDiEdge a Integer] -> [WDiEdge a Integer]
updateEdges [] _ edges = edges
updateEdges ((E f w t):ps) p edges = updateEdges ps p (updateEdge f t p edges)


addFlow :: (Eq a) => a -> a -> Integer -> [WDiEdge a Integer] -> [WDiEdge a Integer]
addFlow x y p ((E f w t):ss) | perteneceX_Y = (E x (w+p) y):ss
                             | perteneceYPX = ss
                             | perteneceYwX = (E x (p-w) y):ss
                             | perteneceYWX = (E x (w-p) y):ss
                             | otherwise    = addFlow x y p ss
  where
    perteneceX_Y = x == f && y == t
    perteneceYPX = y == f && x == t && w == p
    perteneceYwX = y == f && x == t && w < p
    perteneceYWX = y == f && x == t && w > p


addFlows :: (Eq a) => Path (WDiEdge a Integer) -> Integer -> [WDiEdge a Integer] -> [WDiEdge a Integer]
addFlows [] _ sol = sol
addFlows ((E f w t):ps) p sol = addFlows ps p (addFlow f t p sol)


fordFulkerson :: (Ord a) => (WeightedDiGraph a Integer) -> a -> a -> [WDiEdge a Integer]
fordFulkerson g src dst | src == dst = [(E src 0 src)]
                        | otherwise  = undefined


maxFlow :: (Ord a) => [WDiEdge a Integer] -> a -> Integer
maxFlow sol src = sum [ w | (E f w t) <- sol , f == src ]


maxFlowMinCut :: (Ord a) => (WeightedDiGraph a Integer) -> a -> a -> [a] -> Integer
maxFlowMinCut g src dst set = undefined



-- A partir de aquí hasta el final
-- SOLO para alumnos a tiempo parcial
-- sin evaluación continua

localEquilibrium :: (Ord a) => WeightedDiGraph a Integer -> a -> a -> Bool
localEquilibrium = undefined

sourcesAndSinks :: (Eq a) => WeightedDiGraph a b -> ([a],[a])
sourcesAndSinks = undefined

unifySourceAndSink :: (Eq a) => WeightedDiGraph a Integer -> a -> a -> WeightedDiGraph a Integer
unifySourceAndSink = undefined
