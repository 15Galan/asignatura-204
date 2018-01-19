-------------------------------------------------------------------------------
-- Maxiphobic Heaps
--
-- see: Fun with Binary Heap Trees
--      & Alternatives to two Classic Data Structures
--      by Chris Okasaki
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module DataStructures.Heap.MaxiphobicHeap
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

  , drawOnWith
  ) where

import DataStructures.Graphics.DrawTrees
import Test.QuickCheck

data Heap a  = Empty | Node a Int (Heap a) (Heap a) deriving Show

-- number of elements
size :: Heap a -> Int
size Empty            = 0
size (Node _ sz _ _)  = sz

empty :: Heap a
empty  = Empty

isEmpty :: Heap a -> Bool
isEmpty Empty  = True
isEmpty _      = False

singleton :: a -> Heap a
singleton x  = Node x 1 Empty Empty

insert :: (Ord a) => a -> Heap a -> Heap a
insert x h  = merge (singleton x) h

minElem :: Heap a -> a
minElem Empty           = error "minElem on empty heap"
minElem (Node x _ _ _)  = x

delMin :: (Ord a) => Heap a -> Heap a
delMin Empty             = error "delMin on empty heap"
delMin (Node _ _ lh rh)  = merge lh rh

----------------------------------------------------------
-- VVVVVVVVVVVV-SOLO TOCAR ABAJO-VVVVVVVVVVVV--------------
---------------------------------------------------------
-- recursively merges smallest subheaps. Achieves O(log n) complexity
merge :: (Ord a) => Heap a -> Heap a -> Heap a
merge Empty h'     = h'
merge h     Empty  = h
merge h@(Node x sz lh rh) h'@(Node x' sz' lh' rh')
 | x < x'          = aux x lh rh h' 
 | otherwise       = aux x' h lh' rh' 
    where
        aux x h1 h2 h3    = Node x (sz+sz') (merge sub1 sub2) max

            where
                (max, sub1, sub2) = triSort (h1,h2,h3)


triSort :: (Heap a, Heap a, Heap a) -> (Heap a, Heap a, Heap a)
triSort (x,y,z) | mX = (x,y,z)
                | mY = (y,x,z)
                | mZ = (z,x,y)
    where
        mX = (size x >= size y) && (size x >= size z)
        mY = (size y >= size x) && (size y >= size z)
        mZ = (size z >= size x) && (size z >= size y)
----------------------------------------------------------
-- ^^^^^^^^^^^^^^-- SOLO TOCAR ARRIBA ^^^^^^^^^^^ ---------
----------------------------------------------------------

-- Efficient O(n) bottom-up construction for heaps
mkHeap :: (Ord a) => [a] -> Heap a
mkHeap []  = empty
mkHeap xs  = mergeLoop (map singleton xs)
  where
    mergeLoop [h]  = h
    mergeLoop hs   = mergeLoop (mergePairs hs)

    mergePairs []         = []
    mergePairs [h]        = [h]
    mergePairs (h:h':hs)  = merge h h' : mergePairs hs

-------------------------------------------------------------------------------
-- Generating arbritray Heaps
-------------------------------------------------------------------------------

instance (Ord a, Arbitrary a) => Arbitrary (Heap a) where
  arbitrary  = do
    xs <- arbitrary
    return (mkHeap xs)

-------------------------------------------------------------------------------
-- Invariants
-------------------------------------------------------------------------------

isHeap :: (Ord a) => Heap a -> Bool
isHeap Empty             = True
isHeap (Node x _ lh rh)  = x `lessEq` lh && x `lessEq` rh
                           && isHeap lh && isHeap rh
 where
  x `lessEq` Empty            = True
  x `lessEq` (Node x' _ _ _)  = x<=x'


-------------------------------------------------------------------------------
-- Drawing a Heap
-------------------------------------------------------------------------------

instance Subtrees (Heap a) where
  subtrees Empty             = []
  subtrees (Node _ _ lh rh)  = [lh,rh]

  isEmptyTree  = isEmpty

instance (Show a) => ShowNode (Heap a) where
  showNode (Node x _ _ _) = show x

drawOnWith :: FilePath -> (a -> String) -> Heap a -> IO ()
drawOnWith file toString = _drawOnWith file showHeap
 where
  showHeap (Node x _ _ _) = toString x
