-------------------------------------------------------------------------------
-- Axioms priority queue with FIFO behavior for equal priorities
--
-- Data Structures. Grado en Informática. UMA.
-- Blas Ruiz, Noviembre, 2012
-------------------------------------------------------------------------------

module DataStructures.PriorityQueue.PriorityQueueFIFOAxioms where

-- import DataStructures.PriorityQueue.MaxiphobicHeapPriorityQueue
-- import DataStructures.PriorityQueue.WBLeftistHeapPriorityQueue
-- import DataStructures.PriorityQueue.LinearPriorityQueue
import DataStructures.PriorityQueue.PriorityQueueFIFO
import Test.QuickCheck

-----------------
-- axiomas comunes para colas
-----------------
q1        = True    ==>  isEmpty empty
q2 x q    = True    ==>  not (isEmpty (enqueue x q))
q3 x      = True    ==>  first (enqueue x empty) == x
q4 x      = True    ==>  dequeue (enqueue x empty) == empty

-----------------
-- axiomas para colas con prioridad
-----------------

pq5 x y q  = x <= y  ==>  first (enqueue y (enqueue x q)) == first (enqueue x q)
pq6 x y q  = x <= y  ==>  dequeue (enqueue y (enqueue x q)) == enqueue y (dequeue (enqueue x q))
pq7 x y q  = True    ==>  enqueue x (enqueue y q) == enqueue y (enqueue x q)
-- este ultimo deja de verificarse para <= no ANTISIM
-- si lo eliminamos la axiomática no es completa
-- (no todas las colas son "reducibles" a través de los axiomas)
-- hay que introducir nuevos axiomas para la completitud

-- type Elem = Int
type Elem = Persona

pq = priorityQueueAxioms
priorityQueueAxioms = do
  putStrLn " "
  putStrLn "Comprobando axiomas para Priority Queue "
  putStrLn "----------------------------------------"
  putStrLn "Comprobando q1,q2,q3,q4"
  quickCheck (q1 :: Property)
  quickCheck (q2 :: Elem -> PQueue Elem  -> Property)
  quickCheck (q3 :: Elem -> Property)
  quickCheck (q4 :: Elem -> Property)
  putStrLn "Comprobando pq5,pq6"
  quickCheck (pq5 :: Elem -> Elem -> PQueue Elem -> Property)
  quickCheck (pq6 :: Elem -> Elem -> PQueue Elem -> Property)
  putStrLn "Comprobando pq7 : enqueue x (enqueue y q) == enqueue y (enqueue x q)"
  quickCheck (pq7 :: Elem -> Elem -> PQueue Elem -> Property)

-----------------
-- axiomas nuevos para comportamiento SIFO-FIFO
--  con estos axiomas la axiomática es completa
-- (las colas son computables o "reducibles" a través de los axiomas)
-----------------
pqf5 x y q  = True   ==>  first (enqueue y (enqueue x q)) ==
                         first (enqueue (min x y) q)
pqf6 x y q  = True   ==>  dequeue (enqueue y (enqueue x q)) ==
                         enqueue (max x y) (dequeue (enqueue (min x y) q))

fromList :: (Ord a) => [a] -> PQueue a

fromList = foldr enqueue empty

data Persona = P Edad Nombre deriving (Eq,Show)

instance Ord Persona where
     p <= p'    = edad p <= edad p'
-- p<=p' && p'<=p NO IMPLICA NECESARIAMENTE p==p'
-- el axioma de permutación PRODUCE problemas
{-
*DataStructures.PriorityQueue.PriorityQueueFIFOAxioms > let  (x,y,q) = (P 1 True, P 1 False, empty) in
   enqueue x (enqueue y q) == enqueue y (enqueue x q)
False
-}

type Edad = Int; type Nombre = Bool -- String
edad (P e n) = e


instance Arbitrary Persona where
    arbitrary =  do
      edad <- arbitrary
      nombre <- arbitrary
      return (P edad nombre)

pqf = priorityQueueFIFOAxioms
priorityQueueFIFOAxioms = do
  putStrLn " "
  putStrLn "Comprobando axiomas para Priority Queue FIFO"
  putStrLn "--------------------------------------------"
  putStrLn "> Comprobando q1,q2,q3,q4"
  quickCheck (q1 :: Property)
  quickCheck (q2 :: Elem -> PQueue Elem  -> Property)
  quickCheck (q3 :: Elem -> Property)
  quickCheck (q4 :: Elem -> Property)
  putStrLn "> Comprobando pqf5,pqf6"
  quickCheck (pqf5 :: Elem -> Elem -> PQueue Elem -> Property)
  quickCheck (pqf6 :: Elem -> Elem -> PQueue Elem -> Property)

queueToList :: (Ord a) => PQueue a -> [a]
queueToList q
  | isEmpty q = []
  | otherwise   = first q  : queueToList (dequeue q)

ordenada [] = True
ordenada [x] = True
ordenada (x:y:xs) = x <=y && ordenada  (y:xs)

--  queueToListZ :: (Ord a) => Z.PQueue a -> [a]
--  queueToListZ q
  --  | Z.isEmpty q = []
  --  | otherwise   = Z.first q  : queueToListZ (Z.dequeue q)
