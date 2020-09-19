-------------------------------------------------------------------------------
-- Examples for different algorithms on Graphs
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

import DataStructures.Graph.Graph
import DataStructures.Graph.GraphBFT
import DataStructures.Graph.GraphDFT
import DataStructures.Graph.ConnectedComponents
import DataStructures.Util.Random

g1 :: Graph Int
g1 = mkGraphSuc [1,2,3,4] suc
	 where
	  suc 1 = [2,3,5]
	  suc 2 = [1,3]
	  suc 3 = [1,2,4]
		suc 4 = [3]

g1' :: Graph Int
g1' = mkGraphEdges [1,2,3,4] [(1,2), (1,3), (2,3), (3,4)]


demo1 = dftConnectedComps g1

demo2 = bft g1 1

demo3 = bftPaths g1 1


comp :: Int -> Graph Int
comp n = mkGraphEdges vertices edges
  where
    vertices = [1 .. n]
    edges = [ (x,y) | x <- [1..n], y <- [x+1..n] ]


rand :: Int -> Double -> Seed -> Graph Int
rand n p s = mkGraphEdges vertices edges
  where
    vertices = [1 .. n]
    edges = [ edge
            | (edge,r) <- zip [ (x,y) | x <- [1..n], y <- [x+1..n] ] (randoms s)
            , r <= p
            ]


g2 :: Graph Int
g2 = rand 10 0.5 0

demo4 = dftPaths g2 1

g3 :: Graph Int
g3 = rand 10 0.15 1

demo5 = dftConnectedComps g3
