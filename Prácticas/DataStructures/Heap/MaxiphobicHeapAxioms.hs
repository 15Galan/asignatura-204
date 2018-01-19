-- to be completed

module DataStructures.Heap.MaxiphobicHeapAxioms where

import DataStructures.Heap.MaxiphobicHeap
import Test.QuickCheck
import Data.List((\\))

p1 h     = True     ==>  isHeap (empty `asTypeOf` h)
p2 x h   = True     ==>  not (isEmpty (insert x h))
p3 x h   = isHeap h ==>  isHeap (insert x h)
p4 x     = True     ==>  minElem (insert x empty) == x
p5 x h   = not (isEmpty h) && x>m ==> minElem (insert x h) == m
 where m = minElem h
p6 x     = True     ==>  isEmpty (delMin (insert x empty))
p7 h     = not (isEmpty h) && isHeap h ==>  isHeap (delMin h)
p8 h     = not (isEmpty h) && not (isEmpty h') ==>  minElem h <= minElem h'
 where h' = delMin h
p9 xs    = True     ==>  isSorted ys && sameElements xs ys
 where
  h = mkHeap xs
  ys = toList h
p10 x x' h   = True     ==>  toList (insert x (insert x' h)) `sameElements` toList (insert x' (insert x h))

isSorted :: (Ord a) => [a] -> Bool
isSorted []       =  True
isSorted [x]      =  True
isSorted (x:y:zs) =  x<=y && isSorted (y:zs)

sameElements :: (Eq a) => [a] -> [a] -> Bool
sameElements xs ys = null (xs\\ys) && null(ys\\xs)

toList :: (Ord a) => Heap a -> [a]
toList h
 | isEmpty  h = []
 | otherwise  = minElem h : toList (delMin h)

type Elem = Int

main = do
  quickCheck (p1 :: Heap Elem  -> Property)
  quickCheck (p2 :: Elem -> Heap Elem  -> Property)
  quickCheck (p3 :: Elem -> Heap Elem  -> Property)
  quickCheck (p4 :: Elem -> Property)
  quickCheck (p5 :: Elem -> Heap Elem  -> Property)
  quickCheck (p6 :: Elem -> Property)
  quickCheck (p7 :: Heap Elem  -> Property)
  quickCheck (p8 :: Heap Elem  -> Property)
  quickCheck (p9 :: [Elem] -> Property)
  quickCheck (p10 :: Elem -> Elem -> Heap Elem  -> Property)
