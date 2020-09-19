-------------------------------------------------------------------------------
-- Simple client module using a Stack
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module Demos.Stack.StackDemo where

import DataStructures.Stack.LinearStack
import DataStructures.Stack.StackAxioms(stackAxioms)


s1 :: Stack Int
s1 = push 3 (push 2 (push 1 empty))

size :: Stack a -> Int
size s
  | isEmpty s =  0
  | otherwise =  1 + size (pop s)
