-------------------------------------------------------------------------------
-- DiGraph defined by list of vertices and succesors function
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module DataStructures.Graph.DiGraph   
  ( DiGraph
  , DiEdge((:->))
  , Path
  , mkDiGraphEdges   
  , mkDiGraphSuc   
  , vertices
  , diEdges     
  , successors
  , predecesors
  , inDegree 
  , outDegree
  ) where

import List(nub)

data DiEdge a =  a :-> a deriving Show

type Path a = [a] -- Path represented as list of vertices

data DiGraph a = DG [a] (a -> [a])

mkDiGraphSuc :: [a] -> (a -> [a]) -> DiGraph a
mkDiGraphSuc vs suc = DG vs suc

mkDiGraphEdges :: (Eq a) => [a] -> [DiEdge a] -> DiGraph a
mkDiGraphEdges vs es = DG vs suc
 where
   suc v = [ y | x :-> y <- es, x==v ]              

successors :: DiGraph a -> a -> [a]
successors (DG vs suc) v = suc v 

predecesors :: (Eq a) => DiGraph a -> a -> [a]
predecesors (DG vs suc) v = [ w | w <- vs, v `elem` suc w ]

vertices :: DiGraph a -> [a]
vertices (DG vs suc) = vs

diEdges :: DiGraph a -> [DiEdge a]
diEdges (DG vs suc) = [ v :-> w | v <- vs, w <- suc v ] 

outDegree :: DiGraph a -> a -> Int
outDegree g v = length (successors g v)

inDegree :: (Eq a) => DiGraph a -> a -> Int
inDegree g v = length (predecesors g v)

instance Show (DiGraph a) where
  show g = "DiGraph"