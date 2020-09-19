-------------------------------------------------------------------------------
-- This data type can be used for doing tests with QuickCheck.
-- It's cardinality is high enough to generate interesting test cases
-- for most properties and, at the same time, low enough to avoid many
-- "Gave up!" messages.
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2015
-------------------------------------------------------------------------------

module DataStructures.Util.TestDatatype(Elem) where

import Test.QuickCheck

data Elem = A | B | C | D | E | F | G | H | I | J
            deriving (Eq, Ord, Enum, Show, Read)

instance Arbitrary Elem where
  arbitrary = oneof (map return [A,B,C,D,E,F,G,H,I,J])
