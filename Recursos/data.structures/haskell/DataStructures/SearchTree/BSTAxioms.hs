-------------------------------------------------------------------------------
-- Some properties for Binary Search Trees
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2011
-------------------------------------------------------------------------------

module DataStructures.SearchTree.BSTAxioms(ax1,ax2,ax3,ax4,ax5,ax6,ax7,ax8,ax9,ax10,ax11,ax12,bstAxioms) where

import DataStructures.SearchTree.BST
import Data.List((\\),nub)
import Test.QuickCheck

-- BST a invariants
ax1 t       =  True      ==>  isBST (empty `asTypeOf` t)
ax2 x t     =  isBST t   ==>  isBST (insert x t)
ax3 x t     =  isBST t   ==>  isBST (delete x t)

-- Container properties
ax4         =  True    ==>  isEmpty empty
ax5 x t     =  True    ==>  not (isEmpty (insert x t))

ax6 x       =  True    ==>  not (isElem x empty)
ax7 x x' t  =  True    ==>  isElem x' (insert x t) == (x'==x) || isElem x' t
ax8 x t     =  True    ==>  search x (insert x t) == Just x
ax9 x x' t  =  True    ==>  (search x' (insert x t) == Just x) == (x' == x  ||  search x' t == Just x)
ax10 x t    =  True    ==>  not (isElem x (delete x t))

-- Sorted tree properties
ax11 xs       =  True  ==>  isSorted ys && sameElements (nub xs) ys
 where
  keys xs = map fst xs
  t = mkBST xs
  ys = inOrder t

isSorted :: (Ord a) => [a] -> Bool
isSorted []       =  True
isSorted [x]      =  True
isSorted (x:y:zs) =  x<=y && isSorted (y:zs)

sameElements :: (Eq a) => [a] -> [a] -> Bool
sameElements xs ys = null (xs\\ys) && null(ys\\xs)

-- minim and maxim correctness
ax12 t = not (isEmpty t) ==> minim t == minimum xs && maxim t == maximum xs
 where
   xs = inOrder t

type Elem = Int

bstAxioms = do
  quickCheck ( ax1 :: BST Elem -> Property)
  quickCheck ( ax2 :: Elem -> BST Elem -> Property)
  quickCheck ( ax3 :: Elem -> BST Elem -> Property)
  quickCheck ( ax4 :: Property)
  quickCheck ( ax5 :: Elem -> BST Elem -> Property)
  quickCheck ( ax6 :: Elem -> Property)
  quickCheck ( ax7 :: Elem -> Elem -> BST Elem -> Property)
  quickCheck ( ax8 :: Elem -> BST Elem -> Property)
  quickCheck ( ax9 :: Elem -> Elem -> BST Elem -> Property)
  quickCheck (ax10 :: Elem -> BST Elem -> Property)
  quickCheck (ax11 :: [Elem] -> Property)
  quickCheck (ax12 :: BST Elem -> Property)

