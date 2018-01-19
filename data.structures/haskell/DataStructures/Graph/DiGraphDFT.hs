-------------------------------------------------------------------------------
-- Depth First Traversal of a DiGraph
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module DataStructures.Graph.DiGraphDFT
  ( dft
  , dftPaths
  ) where

import DataStructures.Graph.DiGraph
import DataStructures.Stack.LinearStack
import qualified DataStructures.Set.BSTSet as S
import qualified DataStructures.Dictionary.BSTDictionary as D
import DataStructures.Graph.Utils


dft :: (Ord a) => DiGraph a -> a -> [a]
dft g v0  = aux S.empty (push v0 empty)
 where
  aux visited stack
   | isEmpty stack         = [] -- end of traversal
   | v `S.isElem` visited  = aux visited stack' -- v was already visited
   | otherwise             = v : aux visited' (pushAll stack' us)
   where
     v  = top stack
     stack'  = pop stack
     visited'  = S.insert v visited
     us  = [ u | u <- successors g v, u `S.notIsElem` visited ]


-- Returns paths from source to all vertices that are
-- visited during depth first traversal
dftPaths :: (Ord a) => DiGraph a -> a -> [Path a]
dftPaths g v0  = aux S.empty (push (v0 :-> v0) empty) D.empty
 where
  aux visited stack dict
   | isEmpty stack         = [] -- end of traversal
   | v `S.isElem` visited  = aux visited stack' dict -- v is already visited
   | otherwise             =
       pathFromTo v0 v dict' : -- v has been visited. Return path to it
         aux visited' (pushAll stack' es) dict'
   where
     w :-> v  = top stack
     stack'  = pop stack
     visited'  = S.insert v visited
     dict'  = D.insert v w dict -- parent of v is w
     es  = [ v :-> u | u <- successors g v, u `S.notIsElem` visited ]


