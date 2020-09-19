module EulerianCycleDemo where

import DataStructures.Graph.EulerianCycle
import DataStructures.Graph.Graph

-- | functions to check results

{-
  Graphs g3, g5, g6, and g10 are eulerian.

  To check the result for a particular graph (for example, g3) just evaluate:

    'checkEulerianCycle g3 (eulerianCycle g3)'

  To check them all just evaluate:

     'checkThemAll'


  Graphs g2n, g6n, and g7n are not eulerian, 'isEulerian' must return 'False' for them all.
-}

checkThemAll :: IO ()
checkThemAll =
    putStr $ unlines [ "Tests for graph " ++ name ++ ":\n" ++
                       "cycle: " ++  show cycle ++ "\n" ++
                       "length of cycle: " ++ show (length cycle) ++ "\n" ++
                        checkEulerianCycle graph cycle | (name, graph) <- graphs, let cycle = eulerianCycle graph]
    where
      graphs = [("g3", g3), ("g5", g5), ("g6", g6), ("g10", g10)]

checkEulerianCycle graph cycle =
    unlines $ map passOrFail
    [ ("cycle length test: ", cycleLength graph cycle),
      ("cycle is cyclic test: ", pathIsCyclic cycle),
      ("cycle is a path test: ", isPath graph cycle),
      ("origin occurrences test: ", originOcurrences graph cycle),
      ("vertex occurrences test: ", vertexOccurrences graph cycle)
    ]
  where
    passOrFail (xs, True) = xs ++ "PASSED"
    passOrFail (xs, False) = xs ++ "FAILED"

cycleLength :: Eq a => Graph a -> Path a -> Bool
cycleLength g cs = length (edges g) == length cs - 1

pathIsCyclic :: Eq a => Path a -> Bool
pathIsCyclic cs = head cs == last cs

isPath :: Eq a => Graph a -> Path a -> Bool
isPath g cs = null [ y | (x,y) <- zip cs (tail cs), y `notElem` successors g x ]

occurrences :: Eq a => a -> [a] -> Int
occurrences x xs = length [ 1 | y <- xs, x == y ]

originOcurrences :: Eq a => Graph a -> Path a -> Bool
originOcurrences g (c:cs) = degree g c == 2 * occurrences c cs

vertexOccurrences :: Eq a => Graph a -> Path a -> Bool
vertexOccurrences g cs =
    and [ degree g v == 2 * occurrences v cs | v <- vertices g, v /= head cs ]

---------------------
--- EXAMPLES --------
---------------------

data Vertex = A | B | C | D | E | F | G | H | I | J deriving (Show,Eq,Enum,Ord)

g0n :: Graph Vertex -- not eulerian
g0n = mkGraphEdges [] []

g1 :: Graph Vertex -- eulerian
g1 = mkGraphEdges [A] []

g2n :: Graph Vertex -- not eulerian
g2n = mkGraphSuc [A,B] suc
    where
      suc A = [B]
      suc B = [A]

g3 :: Graph Vertex -- eulerian
g3 = mkGraphEdges [A .. C]
                  [(A,B), (B,C), (C,A)]

g5 :: Graph Vertex
g5 = mkGraphEdges [A .. E]
                  [(A, C), (A, D), (B, C), (B, E), (C, D), (C, E)]

g6 :: Graph Vertex
g6 = mkGraphSuc vertices suc -- eulerian
    where
      vertices = [A .. F]
      suc A = [B,E]
      suc B = [A,C,D,E]
      suc C = [B,D,E,F]
      suc D = [B,C,E,F]
      suc E = [A,B,C,D]
      suc F = [C,D]

{-
  A--B--D--F
     |  |
     C--E
-}
g6n  = mkGraphEdges vertices edges -- not eulerian
    where
      vertices =  [A .. F]
      edges = [(A,B),(B,C),(B,D),(D,E),(D,F),(C,E)]

{-
  A--B--D--F
   \ |  |
     C--E--G
-}
g7n  = mkGraphSuc vertices suc -- not eulerian
    where
      vertices = [A .. G]
      suc A = [B,C]
      suc B = [A,C,D]
      suc C = [A,B,E]
      suc D = [B,F,E]
      suc E = [C,D,G]
      suc F = [D]
      suc G = [E]

g10 = mkGraphSuc vertices suc -- eulerian
    where
      vertices = [A .. J]
      suc A = [E,J,I,B]
      suc B = [A,I,H,C]
      suc C = [D,G,H,B]
      suc D = [E,F,G,C]
      suc E = [D,F,J,A]
      suc F = [E,D,G,J]
      suc G = [D,C,H,F]
      suc H = [G,I,C,B]
      suc I = [A,B,J,H]
      suc J = [E,F,A,I]
