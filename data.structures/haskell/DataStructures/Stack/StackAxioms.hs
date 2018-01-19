-------------------------------------------------------------------------------
-- Axioms for a Stack
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2011
-------------------------------------------------------------------------------

module DataStructures.Stack.StackAxioms(ax1,ax2,ax3,ax4,stackAxioms) where

import DataStructures.Stack.LinearStack -- StackOnList
import Test.QuickCheck

-- top returns last pushed element
ax1 x s =  True ==>  top (push x s) == x

-- poping after pushing returns original stack
ax2 x s =  True ==>  pop (push x s) == s

-- an empty stack is empty
ax3     =  True ==>  isEmpty empty

-- push returns an non empty stack
ax4 x s =  True ==>  not (isEmpty (push x s))

type Elem = Int -- Test axioms using stacks of Ints

stackAxioms = do
  quickCheck (ax1 :: Elem -> Stack Elem -> Property)
  quickCheck (ax2 :: Elem -> Stack Elem -> Property)
  quickCheck (ax3 :: Property)
  quickCheck (ax4 :: Elem -> Stack Elem -> Property)
