-------------------------------------------------------------------------------
-- Estructuras de Datos. Grado en Informática, IS e IC. UMA.
-- Examen de Febrero 2015.
--
-- Implementación del TAD Deque
--
-- Apellidos:    Galán Herrera
-- Nombre:       Antonio J.
-- Grado en Ingeniería [INFORMÁTICA]
-- Grupo: A
-------------------------------------------------------------------------------

module TwoListsDoubleEndedQueue
   ( DEQue
   , empty
   , isEmpty
   , first
   , last
   , addFirst
   , addLast
   , deleteFirst
   , deleteLast
   ) where

import Prelude hiding (last)
import Data.List(intercalate)
import Test.QuickCheck

data DEQue a = DEQ [a] [a]

x = DEQ [] [8,7,6,5]
y = DEQ [1,2,3] [4]
z = DEQ [2,8,6] []


-- Complexity: O(1)
empty :: DEQue a
empty = DEQ [] []


-- Complexity: O(1)
isEmpty :: DEQue a -> Bool
isEmpty (DEQ [] _) = True
isEmpty _          = False


-- Complexity: O(1)
addFirst :: a -> DEQue a -> DEQue a
addFirst x (DEQ l1 l2) = (DEQ (x:l1) l2)


-- Complexity: O(1)
addLast :: a -> DEQue a -> DEQue a
addLast x (DEQ l1 l2) = (DEQ l1 (x:l2))


-- Complexity: O(1)
first :: DEQue a -> a
first (DEQ (l:l1) l2) = l


-- Complexity: O(1)
last :: DEQue a -> a
last (DEQ l1 (l:l2)) = l


-- AUXILIAR
corregir :: DEQue a -> DEQue a
corregir (DEQ [] y) = (DEQ x' y')
    where
        mitad = div (length y) 2
        x' = reverse $ drop mitad y
        y' = take mitad y
corregir (DEQ x []) = (DEQ x' y')
    where
        mitad = div (length x) 2
        x' = take mitad x
        y' = reverse $ drop mitad x

{-
corregir' :: DEQue a -> ([a],[a])
corregir' (DEQ x y) = (x',y')
    where
        (DEQ x' y') = corregir (DEQ x y)
-}

-- Complexity: O(n)
deleteFirst :: (Eq a) => DEQue a -> DEQue a
deleteFirst (DEQ [] y) = if (DEQ [] y) == empty then empty else deleteFirst $ corregir (DEQ [] y)
deleteFirst (DEQ (x:xs) y)  = (DEQ xs y)


-- Complexity:
deleteLast :: (Eq a) => DEQue a -> DEQue a
deleteLast (DEQ x (y:ys)) = (DEQ x ys)
deleteLast (DEQ x []) = if (DEQ x []) == empty then empty else (DEQ x' [])
    where
        x' = tail $ reverse x


instance (Show a) => Show (DEQue a) where
   show q = "TwoListsDoubleEndedQueue(" ++ intercalate "," [show x | x <- toList q] ++ ")"

toList :: DEQue a -> [a]
toList (DEQ xs ys) =  xs ++ reverse ys

instance (Eq a) => Eq (DEQue a) where
   q == q' =  toList q == toList q'

instance (Arbitrary a) => Arbitrary (DEQue a) where
   arbitrary =  do
      xs <- listOf arbitrary
      ops <- listOf (oneof [return addFirst, return addLast])
      return (foldr id empty (zipWith ($) ops xs))
