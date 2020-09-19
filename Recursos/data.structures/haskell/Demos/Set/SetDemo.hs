-------------------------------------------------------------------------------
-- Simple client module using a Set
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module Demos.Set.SetDemo where

import DataStructures.Set.SortedLinearSet
import DataStructures.Set.SetAxioms(setAxioms)


s1 :: Set Int
s1 = insert 1 (insert 2 (insert 3 (insert 1 empty)))


-- Iterating over elements of a set

setToList :: Set a -> [a]
setToList = fold (:) []

sumSet :: (Num a) => Set a -> a
sumSet = fold (+) 0
