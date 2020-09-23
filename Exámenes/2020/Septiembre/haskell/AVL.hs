{------------------------------------------------------------------------------
 - Student's name:
 -
 - Student's group:
 -----------------------------------------------------------------------------}

module AVL
  (
    Weight
  , Capacity
  , AVL (..)
  , Bin
  , emptyBin
  , remainingCapacity
  , addObject
  , maxRemainingCapacity
  , height
  , nodeWithHeight
  , node
  , rotateLeft
  , addNewBin
  , addFirst
  , addAll
  , toList
  , linearBinPacking
  , seqToList
  , addAllFold
  ) where

type Capacity = Int
type Weight= Int

data Bin = B Capacity [Weight]

data AVL = Empty | Node Bin Int Capacity AVL AVL deriving Show



-- Un cubo con una capacidad dada, pero vacio
emptyBin :: Capacity -> Bin
emptyBin c = B c []

-- La capacidad restante de un cubo ya viene indicada
remainingCapacity :: Bin -> Capacity
remainingCapacity (B c l) = c

-- Si el objeto cabe en el cubo, lo inserta; si no, eleva un error
addObject :: Weight -> Bin -> Bin
addObject 0 b = b
addObject p (B c l) | (c-p) >= 0 = B (c-p) (p:l)
                    | otherwise  = error "El objeto no cabe en el cubo"


-- Dado un arbol AVL, definimos su capacidad restante maxima como:
-- La capacidad restante del cubo del arbol con mayor capacidad restante
maxRemainingCapacity :: AVL -> Capacity
maxRemainingCapacity Empty = 0
maxRemainingCapacity (Node b h c Empty Empty) = c
maxRemainingCapacity (Node b h c izq der) = maximum [c, maxRemainingCapacity izq, maxRemainingCapacity der]

-- La altura de un cubo ya viene indicada
height :: AVL -> Int
height Empty = 0
-- height (Node b h c izq der) = 1 + maximum (height izq, height der)
height (Node b h c izq der) = h

--
nodeWithHeight :: Bin -> Int -> AVL -> AVL -> AVL
nodeWithHeight b@(B c l) h izq der = Node b h c' izq der
  where
    c' = maxRemainingCapacity (Node b h c izq der)


node :: Bin -> AVL -> AVL -> AVL
node b Empty Empty = Node b 1 (remainingCapacity b) Empty Empty
node b izq der = nodeWithHeight b h' izq der
  where
    h' = 1 + maximum (height izq, height der)


rotateLeft :: Bin -> AVL -> AVL -> AVL
rotateLeft b Empty Empty = Node b 1 (remainingCapacity b) Empty Empty
rotateLeft b izq der = Node bd hd cd (Node b h' (remainingCapacity b) izq izqd) derd
  where
    h' = maximum (height izq, height der)
    Node bi hi ci izqi deri = izq
    Node bd hd cd izqd derd = der


addNewBin :: Bin -> AVL -> AVL
addNewBin bin (Node b h c Empty Empty) = Node b h c Empty der'
  where
    der' = Node bin 1 (remainingCapacity bin) Empty Empty
addNewBin bin (Node b h c Empty der) = addNewBin b der
-- Falta actualizar la altura


addFirst :: Capacity -> Weight -> AVL -> AVL
addFirst c p Empty = Node b' 1 (remainingCapacity b') Empty Empty   -- Arbol vacio
  where
    b' = addObject p $ emptyBin c

addFirst c p (Node b h cc izq der) | maxRemainingCapacity izq >= p = Node b h cc (addFirst c p izq) der
                                   | remainingCapacity b >= p      = Node (addObject p b) h cc izq der
                                   | otherwise = Node b h cc izq (addFirst c p der)


addAll:: Capacity -> [Weight] -> AVL
addAll c [] = Node (emptyBin c) 1 c Empty Empty
addAll c [p] = addFirst c p Empty
addAll c (p:ps) = addFirst c p (addAll c ps)

toList :: AVL -> [Bin]
toList Empty = []   -- Arbol vacio
toList (Node b h c Empty Empty) = [b]
toList (Node b h c izq der) = toList izq ++ toList der

{-
	SOLO PARA ALUMNOS SIN EVALUACION CONTINUA
  ONLY FOR STUDENTS WITHOUT CONTINUOUS ASSESSMENT
 -}

data Sequence = SEmpty | SNode Bin Sequence deriving Show

linearBinPacking:: Capacity -> [Weight] -> Sequence
linearBinPacking _ _ = undefined

seqToList:: Sequence -> [Bin]
seqToList _ = undefined

addAllFold:: [Weight] -> Capacity -> AVL
addAllFold _ _ = undefined



{- No modificar. Do not edit -}

objects :: Bin -> [Weight]
objects (B _ os) = reverse os


instance Show Bin where
  show b@(B c os) = "Bin("++show c++","++show (objects b)++")"
