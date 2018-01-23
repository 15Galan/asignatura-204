-------------------------------------------------------------------------------
-- Weighted DiGraph defined by list of vertices and succesors function
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module DataStructures.Graph.WeightedDiGraph   
  ( WeightedDiGraph
  , WeigthedDiEdge(E)
  , Path
  , mkWeigthedDiGraphEdges   
  , mkWeigthedDiGraphSuc   
  , vertices
  , weigthedDiEdges     
  , successors
  , predecesors
  , inDegree 
  , outDegree
  ) where


data WeigthedDiEdge a w = E a w a deriving Show

type Path a = [a] -- Path represented as list of vertices

data WeightedDiGraph a w = WDG [a] (a -> [(a,w)])

mkWeigthedDiGraphSuc :: [a] -> (a -> [(a,w)]) -> WeightedDiGraph a w
mkWeigthedDiGraphSuc vs sucs = WDG vs sucs

mkWeigthedDiGraphEdges :: (Eq a) => [a] -> [WeigthedDiEdge a w] -> WeightedDiGraph a w
mkWeigthedDiGraphEdges vs es = WDG vs sucs
 where
   sucs v = [ (y,w) | E x w y <- es, x==v ]              

successors :: WeightedDiGraph a w -> a -> [(a,w)]
successors (WDG vs sucs) v = sucs v 

predecesors :: (Eq a) => WeightedDiGraph a w -> a -> [(a,w)]
predecesors (WDG vs sucs) u = [ (v,w) | v <- vs, w <- weightFor u (sucs v) ]
 where
   weightFor v []   = []
   weightFor v ((u,w):wus)
     | v==u         = [w]
     | otherwise    = weightFor v wus

vertices :: WeightedDiGraph a w -> [a]
vertices (WDG vs sucs) = vs

weigthedDiEdges :: WeightedDiGraph a w -> [WeigthedDiEdge a w]
weigthedDiEdges (WDG vs sucs) = [ E v w u | v <- vs, (u,w) <- sucs v ] 

outDegree :: WeightedDiGraph a w -> a -> Int
outDegree g v = length (successors g v)

inDegree :: (Eq a) => WeightedDiGraph a w -> a -> Int
inDegree g v = length (predecesors g v)

instance Show (WeightedDiGraph a w) where
  show g = "WeightedDiGraph"