-------------------------------------------------------------------------------
-- Student's name:
-- Student's group:
--
-- Data Structures. Grado en Informática. UMA.
-------------------------------------------------------------------------------

module DataStructures.Graph.EulerianCycle(isEulerian, eulerianCycle) where

import DataStructures.Graph.Graph
import Data.List

--H.1)
isEulerian :: Eq a => Graph a -> Bool
isEulerian g = null [ v | v <- vertices g, (degree g v) `mod` 2 /= 0]

-- H.2)
remove :: (Eq a) => Graph a -> (a,a) -> Graph a
remove g (v,u) = g'''
      where
          g' = deleteEdge g (v,u)
          g'' = if (degree g' v == 0) then deleteVertex g' v else g'
          g''' = if (degree g'' u == 0) then deleteVertex g'' u else g''

-- H.3)
extractCycle :: (Eq a) => Graph a -> a -> (Graph a, Path a)
extractCycle g v0 = aux g v0 [v0]
    where
        aux g v xs
          | u == v0 = (g', u:xs)
          | otherwise = aux g' u  (u:xs)
          where
              u = head (successors g v)
              g'= remove g (v,u)

-- H.4)
connectCycles :: (Eq a) => Path a -> Path a -> Path a
connectCycles [] (y:ys) = y:ys
connectCycles (x:xs) (y:ys)
    | x == y = y:ys ++ xs
    | otherwise = x:(connectCycles xs (y:ys))

-- H.5)
vertexInCommon :: Eq a => Graph a -> Path a -> a
vertexInCommon g cycle = head [v | v <- vertices g, elem v cycle]

-- H.6)
eulerianCycle :: Eq a => Graph a -> Path a
eulerianCycle g
  | isEulerian g = aux g (head (vertices g)) []
  | otherwise = []
    where
      aux g v xs
        | isEmpty g = xs
        | otherwise = aux g' w xs'
        where
          (g', cycle) = extractCycle g v
          xs' = connectCycles xs cycle
          w = vertexInCommon g' xs'
