import DataStructures.Graph.Graph
import DataStructures.Graph.GraphES as G

g1 :: Graph Int
g1 = mkGraphSuc [1,2,3,4] suc
    where
        suc 1 = [2,3]
        suc 2 = [1,3]
        suc 3 = [1,2,4]
        suc 4 = [3]

g1' :: Graph Int
g1' = mkGraphEdges [1,2,3,4] [(1,2),(1,3),(2,3),(3,4)]

{-
g1 y g1':

     1--2
      \ |
        3--4
-}


g2 :: Grafo Int
g2 = crearGrafoSucesores [1,2,3,4] suc
    where
        suc 1 = [2,3]
        suc 2 = [1,3]
        suc 3 = [1,2,4]
        suc 4 = [3]

g2' :: Grafo Int
g2' = crearGrafoAristas [1,2,3,4] [(1,2),(1,3),(2,3),(3,4)]