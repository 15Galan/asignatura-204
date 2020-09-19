-------------------------------------------------------------------------------
-- Ford-Fulkerson Algorithm. Maximal flow for a weighted directed graph.
--
-- Data Structures. Grado en Informática. UMA.
-------------------------------------------------------------------------------

module FordFulkerson where

import DataStructures.Graph.FordFulkerson
import DataStructures.Graph.WeightedDiGraph
import DataStructures.Graph.WeightedDiGraphBFT

{- Some examples and expected results:

> maxFlowPath [E 'A' 3 'B', E 'C' 1 'D', E 'A' 7 'D']
1

> updateEdge 'a' 'b' 7 [E 'a' 5 'c', E 'a' 4 'b', E 'b' 3 'a']
['a'-5->'c','a'-11->'b','b'-3->'a']

> updateEdge 'a' 'b' (-4) [E 'a' 5 'c', E 'a' 4 'b', E 'b' 3 'a']
['a'-5->'c','b'-3->'a']

> updateEdge 'a' 'z' 23 [E 'a' 5 'c', E 'a' 4 'b', E 'b' 3 'a']
['a'-5->'c','a'-4->'b','b'-3->'a','a'-23->'z']

> updateEdges [E 'a' 4 'z', E 'a' 3 'b', E 'c' 1 'd'] 2 [E 'a' 1 'b', E 'b' 2 'c', E 'c' 3 'd']
['a'-3->'b','b'-2->'c','c'-5->'d','a'-2->'z']

-}

g :: WeightedDiGraph String Integer
g = mkWeightedDiGraphSuc ["S","A","B","C","D","E","F","T"] sucs
    where
        sucs "S" = [("C",4) ,("B",8), ("A",4)]
        sucs "A" = [("D",3), ("F",7)]
        sucs "B" = [("E",9)]
        sucs "C" = [("E",5), ("F",2)]
        sucs "D" = [("T",6)]
        sucs "E" = [("T",9)]
        sucs "F" = [("T",5)]
        sucs "T" = []
        
{- Some examples and expected results:

> let list = fordFulkerson g "S" "T"
> list
["F"-5->"T","A"-3->"F","S"-4->"A","E"-9->"T","B"-8->"E","S"-8->"B",
"C"-2->"F","S"-3->"C","C"-1->"E","D"-1->"T","A"-1->"D"]

> maxFlow list "S"
15

> maxFlowMinCut g "S" "T" ["S","A","F"]
15

-}


g2 :: WeightedDiGraph String Integer
g2 = mkWeightedDiGraphSuc ["S","A","B","C","D","T"] sucs
    where
        sucs "S" = [("A",10), ("C",10)]
        sucs "A" = [("B",4), ("C",2), ("D",8)]
        sucs "B" = [("T",10)]
        sucs "C" = [("D",9)]
        sucs "D" = [("B",6), ("T",10)]
        sucs "T" = []
        
{- Some examples and expected results:
>   
> list2
["D"-10->"T","C"-9->"D","S"-9->"C","A"-6->"D","S"-10->"A",
"B"-9->"T","A"-4->"B","D"-5->"B"]

> maxFlow list2 "S"
19

> maxFlowMinCut g2 "S" "T" ["S","A","B","C","D"]
19

-}