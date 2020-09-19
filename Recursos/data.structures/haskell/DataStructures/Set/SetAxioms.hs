-------------------------------------------------------------------------------
-- Axioms for a Set
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2011
-------------------------------------------------------------------------------

module DataStructures.Set.SetAxioms(ax1,ax2,ax3,ax4,ax5,ax6,ax7,ax8,ax9,setAxioms) where

--import DataStructures.Set.LinearSet
import DataStructures.Set.ListSet
--import DataStructures.Set.SortedLinearSet
--import DataStructures.Set.BSTSet
import Test.QuickCheck


type Elem = Int -- Test axioms using sets of Ints

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
