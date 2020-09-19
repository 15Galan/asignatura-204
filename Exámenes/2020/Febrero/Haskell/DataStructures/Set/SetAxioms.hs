-------------------------------------------------------------------------------
-- Axioms for Sets
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2011
-------------------------------------------------------------------------------

module DataStructures.Set.SetAxioms(ax1,ax2,ax3,ax4,ax5,ax6,ax7,ax8,ax9,ax10,setAxioms,extSetAxioms) where

--import DataStructures.Set.WithRepetitions
--import DataStructures.Set.ListSet
import DataStructures.Set.SortedLinearSet
-- import DataStructures.Set.BSTSet
import Test.QuickCheck
import qualified DataStructures.Util.TestDatatype as TDT

-- type Elem = Int -- Test axioms using sets of Ints
type Elem = TDT.Elem


-- Axioms for basic set of operations
ax1     = True ==> isEmpty empty
ax2 x s = True ==> not (isEmpty (insert x s))

ax3 x     = True ==> not (isElem x empty )
ax4 x y s = True ==> isElem x (insert y s) == (x==y) || isElem x s

ax5     = True             ==> size (empty :: Set Elem) == 0
ax6 x s = isElem x s       ==> size (insert x s) == size s
ax7 x s = not (isElem x s) ==> size (insert x s) == 1 + size s

ax8 x      = True ==> delete x empty == empty
ax9 x y s  = x==y ==> delete x (insert y s) == delete x s
ax10 x y s = x/=y ==> delete x (insert y s) == insert y (delete x s)


-- Axioms for extended set of operations
ax11 s        = True ==> union s empty == s
ax12 x s1 s2  = True ==> union s1 (insert x s2) == insert x (union s1 s2)

ax13 s         = True              ==> intersection s empty == empty
ax14 x s1 s2   = isElem x s1       ==> intersection s1 (insert x s2) == insert x (intersection s1 s2)
ax15 x s1 s2   = not (isElem x s1) ==> intersection s1 (insert x s2) == intersection s1 s2

ax16 s        = True ==> difference s empty == s
ax17 x s1 s2  = True ==> difference s1 (insert x s2) == delete x (difference s1 s2)


setAxioms = do
  quickCheck ( ax1 :: Property)
  quickCheck ( ax2 :: Elem -> Set Elem -> Property)
  quickCheck ( ax3 :: Elem -> Property)
  quickCheck ( ax4 :: Elem -> Elem -> Set Elem -> Property)
  quickCheck ( ax5 :: Property)
  quickCheck ( ax6 :: Elem -> Set Elem -> Property)
  quickCheck ( ax7 :: Elem -> Set Elem -> Property)
  quickCheck ( ax8 :: Elem -> Property)
  quickCheck ( ax9 :: Elem -> Elem -> Set Elem -> Property)
  quickCheck (ax10 :: Elem -> Elem -> Set Elem -> Property)

-- Includes axioms for extended set of operations
extSetAxioms = do
  setAxioms
  quickCheck ( ax11 :: Set Elem -> Property)
  quickCheck ( ax12 :: Elem -> Set Elem -> Set Elem -> Property)
  quickCheck ( ax13 :: Set Elem -> Property)
  quickCheck ( ax14 :: Elem -> Set Elem -> Set Elem -> Property)
  quickCheck ( ax15 :: Elem -> Set Elem -> Set Elem -> Property)
  quickCheck ( ax16 :: Set Elem -> Property)
  quickCheck ( ax17 :: Elem -> Set Elem -> Set Elem -> Property)





{-
setAxioms = do
  quickCheck (ax1 :: Property)
  quickCheck (ax2 :: Elem -> Set Elem -> Property)
  quickCheck (ax3 :: Elem -> Set Elem -> Property)
  quickCheck (ax4 :: Elem -> Elem -> Set Elem -> Property)
  quickCheck (ax5 :: Elem -> Property)
  quickCheck (ax6 :: Elem -> Elem -> Set Elem -> Property)
  quickCheck (ax7 :: Elem -> Property)
  quickCheck (ax8 :: Elem -> Elem -> Set Elem -> Property)
  quickCheck (ax9 :: Elem -> Elem -> Set Elem -> Property)


-- the empty set is empty
ax1 = True ==> isEmpty empty

-- insert always returns non-empty sets
ax2 x s = True ==> not (isEmpty (insert x s))

-- an element is only included once in a set
ax3 x s = True ==> insert x (insert x s) == insert x s

-- order of insertion is not important
ax4 x y s = True ==> insert x (insert y s) == insert y (insert x s)

-- no element is included in empty set
ax5 x = True ==> not (isElem x empty)

-- only elements previously inserted are included in set
ax6 x y s = True ==> isElem y (insert x s) == (x==y) || isElem y s

-- deleting a non-included element does not modify set
ax7 x = True ==> delete x empty == empty

-- deleting last inserted element returns set before insertion
ax8 x y s = (x==y) ==> delete x (insert y s) == delete x s

-- delete and insert commute
ax9 x y s = (x/=y) ==> delete x (insert y s) == insert y (delete x s)

-}
