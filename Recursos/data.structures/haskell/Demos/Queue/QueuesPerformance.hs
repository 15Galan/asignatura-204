-------------------------------------------------------------------------------
-- Experimental test for evaluating Queues performance
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module Demos.Queue.QueuesPerformance where

import DataStructures.Util.Random
import DataStructures.Queue.TwoListsQueue -- LinearQueue
import System.CPUTime

data Operation = Enqueue | Dequeue

-- on average, do 2 enqueues for each dequeue
randomOperations :: Seed -> [Operation]
randomOperations s = randomsIn [Enqueue, Enqueue, Dequeue] s

-- forces queue evaluation by summing its elements
sumQ :: (Num a) => Queue a -> a
sumQ q
 | isEmpty q = 0
 | otherwise = first q + sumQ (dequeue q)

test :: Seed -> Int -> Int
test s n = sumQ (foldr simulate empty (take n (randomOperations s)))

simulate :: Operation -> Queue Int -> Queue Int
simulate Enqueue q = enqueue 0 q
simulate Dequeue q = if isEmpty q then q else dequeue q

main = do
 let tests = 10
 let numOperations = 10000
 t0 <- getCPUTime
 let xs = [ test s numOperations | s <- [0..tests-1]]
 print (sum xs) -- force evaluation
 t1 <- getCPUTime
 let average = toSecs (t1-t0) / fromIntegral tests
 putStrLn ("Tests took "++ show average ++ " secs on average")

toSecs :: Integer -> Double
toSecs x = fromIntegral x / 10^12
