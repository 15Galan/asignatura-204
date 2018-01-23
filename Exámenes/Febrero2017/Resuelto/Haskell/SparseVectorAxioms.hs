module SparseVectorAxioms where

import Test.QuickCheck hiding (vector)

import SparseVector

inRange :: (Ord a) => a -> (a,a) -> Bool
inRange x (y,z) = y <= x  &&  x <= z

maxExp :: Int
maxExp = 9 :: Int

ax1 :: Int -> a -> Property
ax1 exp x = inRange exp (0,maxExp) ==> size (vector exp x) == 2^exp

ax2 :: Eq a => Int -> a -> Int -> Property
ax2 exp x i = inRange exp (0,maxExp) && inRange i (0,size v-1) ==> get i v == x
  where
    v = vector exp x

ax3 :: Eq a => Int -> a -> Vector a -> Property
ax3 i x v = inRange i (0,size v-1) ==> get i (set i x v) == x

ax4 :: Eq a => Int -> Int -> a -> Vector a -> Property
ax4 i j x v = inRange i (0,size v-1) && inRange j (0,size v-1) && i /= j ==>
                get i (set j x v) == get i v

type Elem = Char

main :: IO ()
main = do
  quickCheck (ax1 :: Int -> Elem -> Property)
  quickCheck (ax2 :: Int -> Elem -> Int -> Property)
  quickCheck (ax3 :: Int -> Elem -> Vector Elem -> Property)
  quickCheck (ax4 :: Int -> Int -> Elem -> Vector Elem -> Property)
