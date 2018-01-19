-------------------------------------------------------------------------------
-- Priority queue with FIFO behavior for equal priorities
--
-- Seguimos la idea de Pablo López: cada cola SIFO-LIFO encapsula una
-- cola SIFO, cuyos elementos se "enriquecen" con el orden de encolado
-- Un contador "global" es incrementado posteriormente
--
-- Data Structures. Grado en Informática. UMA.
-- Blas Ruiz, Noviembre, 2012
-------------------------------------------------------------------------------

module DataStructures.PriorityQueue.PriorityQueueFIFO
  ( PQueue
  , Cont(..)
  , empty
  , isEmpty
  , first
  , dequeue
  , enqueue
--  , queueToList
  ) where

-- import qualified DataStructures.PriorityQueue.LinearPriorityQueue as PQ
-- import qualified DataStructures.PriorityQueue.MaxiphobicHeapPriorityQueue as PQ
import qualified DataStructures.PriorityQueue.WBLeftistHeapPriorityQueue as PQ
import Data.List(intercalate)
import Test.QuickCheck

data Cont a = C Int a deriving Show
-- el Int es el entero que se le asignó al solicitar enqueue

data PQueue a = PQF Int (PQ.PQueue (Cont a))
-- el Int contabiliza el total de llamadas a enqueue

instance Eq a  => Eq (Cont a) where
     C n x == C m y  = x==y && n==m

instance (Eq a, Ord a) => Ord (Cont a) where
  -- esta definición es muy delicada, ya que
  -- la relación <= puede no ser ANTISIMETRICA
  C n x <= C m y
    | x <=y && y<=x = n <= m -- si tienen la misma prioridad, decide el que entró antes
    | otherwise     = x <= y

empty :: PQueue a
empty  = PQF 0 PQ.empty

isEmpty :: PQueue a -> Bool
isEmpty (PQF n q ) = PQ.isEmpty q

enqueue :: (Ord a) => a -> PQueue a -> PQueue a
enqueue x (PQF n  q)  = PQF (n+1) (PQ.enqueue (C n x) q)
-- adornamos, encolamos e incrementamos el contador de llamadas


first :: PQueue a -> a
first (PQF n q)  = x -- le quitamos el "adorno"
   where C _ x = PQ.first q

dequeue :: (Ord a) => PQueue a -> PQueue a
dequeue (PQF n  q)  =  PQF n (PQ.dequeue q)

queueToList :: (Ord a) => PQ.PQueue a -> [a]
queueToList q
  | PQ.isEmpty q = []
  | otherwise   = PQ.first q  : queueToList (PQ.dequeue q)

-- Showing a priority queue sifo-fifo
instance (Ord a, Show a) => Show (PQueue a) where
--  show (PQF n q)  = "PriorityQueueFIFO(" ++ intercalate "," (map show . queueToList $ q) ++ ")"
  show (PQF n q)  = "PriorityQueueFIFO(" ++ intercalate "," (map show . queueToList $ q) ++ ")"

-- priority queue sifo-fifo equality
instance (Ord a) => Eq (PQueue a) where
  (PQF _ q) == (PQF _ q')  = map info (queueToList q) == map info (queueToList q')
               where info (C _ x) = x

-- This instace is used by QuickCheck to generate random priority queue
instance (Ord a, Arbitrary a) => Arbitrary (PQueue a) where
    arbitrary =  do
      xs <- listOf arbitrary
      return (foldr enqueue empty xs)
