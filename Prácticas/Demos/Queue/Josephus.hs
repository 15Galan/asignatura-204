-------------------------------------------------------------------------------
-- Solution to Josephus' problem using a queue
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module Demos.Queue.Josephus where

import DataStructures.Queue.LinearQueue

createQueue :: Int -> Queue Int
createQueue m = foldr enqueue empty [m-1,m-2..0]

skip :: Int -> Queue Int -> Queue Int
skip 0 q = q
skip n q = skip (n-1) (enqueue x q')
        where x  = first q
              q' = dequeue q

josephus :: Int -> Int -> [Int]
josephus m n = josephus' n (createQueue m)

josephus' :: Int -> Queue Int -> [Int]
josephus' n q
        | isEmpty q = [ ]
        | otherwise = x : josephus' n q''
        where q'  = skip n q
              x   = first q'
              q'' = dequeue q'
