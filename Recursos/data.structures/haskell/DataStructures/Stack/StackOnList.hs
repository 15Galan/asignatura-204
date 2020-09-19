-------------------------------------------------------------------------------
-- Stacks implemented using lists
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2011
-------------------------------------------------------------------------------

module DataStructures.Stack.StackOnList
  ( Stack
  , empty
  , isEmpty
  , push
  , pop
  , top
  ) where

import Data.List(intercalate)
import Test.QuickCheck

data Stack a  = SonL [a]

empty :: Stack a
empty =  SonL []

isEmpty :: Stack a -> Bool
isEmpty (SonL [])  = True
isEmpty _          = False

push :: a -> Stack a -> Stack a
push x (SonL xs)  = SonL (x:xs)

top :: Stack a -> a
top (SonL [])      = error "top on empty Stack"
top (SonL (x:xs))  = x

pop :: Stack a -> Stack a
pop (SonL [])      = error "pop on empty Stack"
pop (SonL (x:xs))  = SonL xs


-- Showing a stack
instance (Show a) => Show (Stack a) where
  show (SonL xs)  = "StackOnList(" ++ intercalate "," (map show xs) ++ ")"

-- Stack equality
instance (Eq a) => Eq (Stack a) where
  (SonL xs) == (SonL xs')  = xs==xs'  -- uses list predefined equality

-- This instance is used by QuickCheck to generate random stacks
instance (Arbitrary a) => Arbitrary (Stack a) where
    arbitrary  = do
      xs <- listOf arbitrary
      return (foldr push empty xs)

