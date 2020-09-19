-------------------------------------------------------------------------------
-- Utils for graph algorithms
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module DataStructures.Graph.Utils
  ( pathFromTo
  , enqueueAll
  , pushAll
  ) where

import DataStructures.Stack.LinearStack
import DataStructures.Queue.TwoListsQueue
import qualified DataStructures.Dictionary.BSTDictionary as D

pathFromTo :: (Ord a) => a -> a -> D.Dictionary a a -> [a]
pathFromTo v0 w src  = reverse (aux w)
 where
  aux w
   | w == v0    = [w]
   | otherwise  = w : aux s
   where Just s  = D.valueOf w src


pushAll :: Stack a -> [a] -> Stack a
pushAll s xs  = foldr push s xs

enqueueAll :: Queue a -> [a] -> Queue a
enqueueAll s xs  = foldr enqueue s xs