-------------------------------------------------------------------------------
-- Weight Biased Leftist Heaps
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module DataStructures.Heap.WBLeftistHeap
  ( Heap
  , empty
  , isEmpty
  , minElem
  , delMin
  , insert
  , merge

  , mkHeap
  , size
  , isHeap
  , isWeightedLeftist

  , drawOnWith
  ) where

import DataStructures.Graphics.DrawTrees
import Test.QuickCheck

data Heap a  = Empty | Node a Int (Heap a) (Heap a) deriving Show

-- number of elements
weight :: Heap a -> Int
weight Empty           = 0
weight (Node _ w _ _)  = w

size :: Heap a -> Int
size = weight

empty :: Heap a
empty  = Empty

isEmpty :: Heap a -> Bool
isEmpty Empty  = True
isEmpty _      = False

minElem :: Heap a -> a
minElem Empty           = error "minElem on empty heap"
minElem (Node x _ _ _)  = x

delMin :: (Ord a) => Heap a -> Heap a
delMin Empty             = error "delMin on empty heap"
delMin (Node _ _ lh rh)  = merge lh rh

singleton :: a -> Heap a
singleton x = Node x 1 Empty Empty

insert :: (Ord a) => a -> Heap a -> Heap a
insert x h = merge (singleton x) h

-- puts always heavier heap on left side
node :: a -> Heap a -> Heap a -> Heap a
node x h h'
 | w >= w'    = Node x s h h'
 | otherwise  = Node x s h' h
 where
   w = weight h
   w' = weight h'
   s = w + w' + 1

-- merges two heaps along their right spines
merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge Empty h'     = h'
merge h     Empty  = h
merge h@(Node x w lh rh) h'@(Node x' w' lh' rh')
 | x <= x'         = node x lh (merge rh h')  -- note that recursive calls use right side heap
 | otherwise       = node x' lh' (merge h rh')

-- Efficient O(n) bottom-up construction for heaps
mkHeap :: (Ord a) => [a] -> Heap a
mkHeap []  = empty
mkHeap xs  = mergeLoop (map singleton xs)
  where
    -- mergeLoop []  = empty
    mergeLoop [h] = h
    mergeLoop hs  = mergeLoop (mergePairs hs)

    mergePairs []         = []
    mergePairs [h]        = [h]
    mergePairs (h:h':hs)  = merge h h' : mergePairs hs

-------------------------------------------------------------------------------
-- Generating arbritray Heaps
-------------------------------------------------------------------------------

instance (Ord a, Arbitrary a) => Arbitrary (Heap a) where
  arbitrary = do
    kvs <- arbitrary
    return (mkHeap kvs)

-------------------------------------------------------------------------------
-- Invariants
-------------------------------------------------------------------------------

isHeap :: (Ord a) => Heap a -> Bool
isHeap Empty             = True
isHeap (Node x w lh rh)  = x `lessEq` lh && x `lessEq` rh
                            && isHeap lh && isHeap rh
 where
  x `lessEq` Empty            = True
  x `lessEq` (Node x' _ _ _)  = x<=x'

isWeightedLeftist :: Heap a -> Bool
isWeightedLeftist Empty             = True
isWeightedLeftist (Node _ _ lh rh)  = weight lh >= weight rh
                                       && isWeightedLeftist lh
                                       && isWeightedLeftist rh

rightSpine :: Heap a -> [a]
rightSpine Empty            = []
rightSpine (Node x _ _ rh)  = x : rightSpine rh

-- length of right spine
lrs :: Heap a -> Int
lrs  = length . rightSpine

-- The length of the right spine is O(log n)
rightSpineIsLog h = lrs h <= log2 (weight h+1)
 where
   log2 :: Int -> Int
   log2 n = truncate (logBase 2 (fromIntegral n))


-------------------------------------------------------------------------------
-- Drawing a Heap
-------------------------------------------------------------------------------

instance Subtrees (Heap a) where
  subtrees Empty             = []
  subtrees (Node x w lh rh)  = [lh,rh]

  isEmptyTree  = isEmpty

instance (Show a) => ShowNode (Heap a) where
  showNode (Node x _ _ _)  = show x

drawOnWith :: FilePath -> (a -> String) -> Heap a -> IO ()
drawOnWith file toString  = _drawOnWith file showHeap
 where
  showHeap (Node x _ _ _)  = toString x
