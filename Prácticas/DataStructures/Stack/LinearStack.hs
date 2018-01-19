-------------------------------------------------------------------------------
-- Last-In First-Out Stack
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module DataStructures.Stack.LinearStack
  ( Stack
  , empty
  , isEmpty
  , push
  , pop
  , top
  ) where

import Data.List(intercalate)
import Test.QuickCheck

data Stack a = Empty | Node a (Stack a)

empty :: Stack a
empty =  Empty

isEmpty :: Stack a -> Bool
isEmpty Empty =  True
isEmpty _     =  False

push :: a -> Stack a -> Stack a
push x s =  Node x s

top :: Stack a -> a
top Empty      =  error "top on empty stack"
top (Node x s) =  x

pop :: Stack a -> Stack a
pop Empty      =  error "pop on empty stack"
pop (Node x s) =  s

-- Showing a stack
instance (Show a) => Show (Stack a) where
  show s = "LinearStack(" ++ intercalate "," (aux s) ++ ")"
    where
     aux Empty      =  []
     aux (Node x s) =  show x : aux s

-- Stack equality
instance (Eq a) => Eq (Stack a) where
  Empty      == Empty        =  True
  (Node x s) == (Node x' s') =  x==x' && s==s'
  _          == _            =  False

-- This instance is used by QuickCheck to generate random stacks
instance (Arbitrary a) => Arbitrary (Stack a) where
    arbitrary =  do
      xs <- listOf arbitrary
      return (foldr push empty xs)

