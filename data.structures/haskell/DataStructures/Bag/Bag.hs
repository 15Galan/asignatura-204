---------------------------------------
-- Antonio J. Galán Herrera
---------------------------------------


module Bag (
	Bag
	, empty
	, isEmpty
	, insert
	, ocurrences
	, delete
	)where

import Data.List


data Bag a = Emtpy | Node a Int (Bag a)


empty :: Bag a
empty = Emtpy

isEmpty :: Bag a -> Bool
isEmpty Empty = True
is Empty _    = False

insert :: (Ord a) => a -> Bag a -> Bag a
insert _ Empty = Node _ 1 Empty
insert x (Node y n b) | x < y  = Node x 1 (Node y n b)
                      | x == y = Node y (n+1) b
                      | x > y  = Node y n (insert x b)

ocurrences :: (Ord a) => a -> Bag a -> Int
ocurrences _ Empty = 0
ocurrences x (Node y n b) = if x == y then n else ocurrences x b

delete :: (Ord a) => a -> Bag a -> Bag a
delete _ Empty = Empty
delete x (Node y n b) | x < y  = Node y n b
                      | x == y = if n > 0 then Node y (n-1) b else b
                      | otherwise       = delete x b

instance (Ord a, Arbitrary a) => Arbitrary (Bag a) where
    arbitrary = do
        xs <- listOf arbitrary
        return (foldr insert empty xs)