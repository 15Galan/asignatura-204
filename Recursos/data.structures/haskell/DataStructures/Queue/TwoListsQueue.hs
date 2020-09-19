-------------------------------------------------------------------------------
-- Efficient functional queue by means of two lists (used as stacks) 
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module DataStructures.Queue.TwoListsQueue
  ( Queue
  , empty
  , isEmpty
  , enqueue
  , first
  , dequeue
  ) where

import Data.List(intercalate)
import Test.QuickCheck

data Queue a =  Q [a] [a]

empty :: Queue a
empty =  Q [] []

mkValid :: [a] -> [a] -> Queue a
mkValid [] ys =  Q (reverse ys) []
mkValid xs ys =  Q xs ys
 
isEmpty :: Queue a -> Bool
isEmpty (Q [] _) =  True
isEmpty _        =  False

enqueue :: a -> Queue a -> Queue a
enqueue x (Q xs ys) =  mkValid xs (x:ys)

first :: Queue a -> a
first (Q [] _)      =  error "first on empty queue"
first (Q (x:xs) ys) =  x

dequeue :: Queue a -> Queue a
dequeue (Q [] _)      =  error "dequeue on empty queue"
dequeue (Q (x:xs) ys) =  mkValid xs ys

toList :: Queue a -> [a]
toList (Q xs ys) =  xs ++ reverse ys

-- Showing a queue
instance (Show a) => Show (Queue a) where
  show q = "TwoListsQueue(" ++ intercalate "," [show x | x <- toList q] ++ ")"

-- Queue equality
instance (Eq a) => Eq (Queue a) where
    q == q' =  toList q == toList q'

-- This instance is used by QuickCheck to generate random queues
instance (Arbitrary a) => Arbitrary (Queue a) where
    arbitrary = do
      xs <- listOf arbitrary
      return (foldr enqueue empty xs)

