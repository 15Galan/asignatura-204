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


emptyBin :: Capacity -> Bin
emptyBin c = B c []


remainingCapacity :: Bin -> Capacity
remainingCapacity (B c l) = c


addObject :: Weight -> Bin -> Bin
addObject p (B c l) | (c-p) >= 0 = (B (c-p) (p:l))
                    | otherwise  = error "El objeto no cabe en el cubo"


maxRemainingCapacity :: AVL -> Capacity
maxRemainingCapacity Empty = 0
maxRemainingCapacity (Node b h c izq der) = c


height :: AVL -> Int
height Empty = 0
height (Node b h c izq der) = h


nodeWithHeight :: Bin -> Int -> AVL -> AVL -> AVL
nodeWithHeight b@(B c l) h Empty Empty = (Node b h c Empty Empty)
nodeWithHeight b h Empty der = der
nodeWithHeight b h izq Empty = izq
nodeWithHeight b@(B c (l:ls)) h izq der | pesaIzq = nodeWithHeight bi hi izqi deri
                                        | pesaDer = nodeWithHeight bd hd izqd derd
  where
    pesaIzq = c1 >= c2
    pesaDer = c1 <  c2
    (Node bi@(B c1 (li:lsi)) hi ci izqi deri) = izq
    (Node bd@(B c2 (ld:lsd)) hd cd izqd derd) = der


node :: Bin -> AVL -> AVL -> AVL
node b@(B c l) Empty Empty = (Node b 1 c Empty Empty)
node b@(B c l) Empty der = (Node b (1 + (height der)) c Empty der)
node b@(B c l) izq Empty = (Node b (1 + (height izq)) c izq Empty)
node b@(B c l) izq der = (Node b h' c' izq der)
  where
    h' = height izq + height der
    c' = maxRemainingCapacity (Node b h'' c izq der)
    h'' = maximum [(height izq), (height der)]


-- Pendiente
rotateLeft :: Bin -> AVL -> AVL -> AVL
rotateLeft = undefined
-- rotateLeft b@(B c l) Empty Empty = (Node b 1 c Empty Empty)
-- rotateLeft b@(B c l) Empty der = (Node bd hd cd (Node bi (1 - (height der)) cd Empty Empty) izqd derd)
-- rotateLeft b@(B c l) izq Empty = (Node bi hi ci (Node bd (1 - (height izq)) ci Empty Empty) izqi deri)
-- rotateLeft b izq der = (Node bd (1+hd) cd izq izqi)
--   where
--     (Node bi@(B c1 (li:lsi)) hi ci izqi deri) = izq
--     (Node bd@(B c2 (ld:lsd)) hd cd izqd derd) = der


addNewBin :: Bin -> AVL -> AVL
addNewBin b@(B c l) Empty = (Node b 1 c Empty Empty)
addNewBin b@(B c l) (Node b2 h2 c2 izq der) = addNewBin b der
-- Falta actualizar la altura

addFirst :: Capacity -> Weight -> AVL -> AVL
addFirst _ _ _ = undefined

addAll:: Capacity -> [Weight] -> AVL
addAll _ _ = undefined

toList :: AVL -> [Bin]
toList _ = undefined

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
