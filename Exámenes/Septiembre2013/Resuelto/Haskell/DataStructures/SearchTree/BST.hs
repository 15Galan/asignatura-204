-------------------------------------------------------------------------------
-- Binary Search Trees
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2011
-------------------------------------------------------------------------------

module DataStructures.SearchTree.BST 
  ( BST 
  , empty
  , isEmpty
  , insert
  , updateOrInsert
  , search
  , isElem
  , delete
  , inOrder
  , preOrder
  , postOrder
  , minim
  , maxim
  , isBST
  , mkBST
  , mapOrAdd

  , pretty
  , drawOnWith
  ) where

import Data.Maybe(isJust)
import DataStructures.Graphics.DrawTrees
import Test.QuickCheck

data BST a = Empty 
           | Node a (BST a) (BST a)
           deriving Show 

empty :: BST a
empty =  Empty

isEmpty :: BST a -> Bool
isEmpty Empty =  True
isEmpty _     =  False

-------------------------------------------------------------------------------
-- Search
-------------------------------------------------------------------------------

search :: (Ord a) => a -> BST a -> Maybe a
search x' Empty  = Nothing
search x' (Node x lt rt)
  | x'<x         = search x' lt
  | x'>x         = search x' rt
  | otherwise    = Just x

isElem :: (Ord a) => a -> BST a -> Bool
isElem x t = isJust (search x t)


-------------------------------------------------------------------------------
-- Insertion
-------------------------------------------------------------------------------

insert :: (Ord a) => a -> BST a -> BST a
insert x' Empty  =  Node x' Empty Empty
insert x' (Node x lt rt)
    | x'<x       =  Node x (insert x' lt) rt
    | x'>x       =  Node x lt (insert x' rt)
    | otherwise  =  Node x' lt rt

updateOrInsert :: (Ord a) => (a -> a) -> a -> BST a -> BST a
updateOrInsert f x' Empty  = Node x' Empty Empty
updateOrInsert f x' (Node x lt rt)
  | x'<x          = Node x (updateOrInsert f x' lt) rt
  | x'>x          = Node x lt (updateOrInsert f x' rt)
  | otherwise     = Node (f x) lt rt

-- insert could be written by using updateOrInsert:
-- insert x' = updateOrInsert (const x') x'

mkBST :: (Ord a) => [a] -> BST a
mkBST xs = foldl (\t x -> insert x t) empty xs

-- Should not modify key value
mapOrAdd :: (Ord a) => (a->a) -> a -> BST a -> BST a
mapOrAdd f x' Empty  =  Node x' Empty Empty
mapOrAdd f x' (Node x l r)
  | x'<x                  =  Node x (mapOrAdd f x' l) r
  | x'>x                  =  Node x l (mapOrAdd f x' r)
  | otherwise             =  Node (f x) l r


-------------------------------------------------------------------------------
-- Deletion
-------------------------------------------------------------------------------

delete :: (Ord a) => a -> BST a -> BST a
delete x' Empty  =  Empty
delete x' (Node x lt rt)
  | x'<x       =  Node x (delete x' lt) rt
  | x'>x       =  Node x lt (delete x' rt)
  | otherwise  =  join lt rt

join :: BST a -> BST a -> BST a
join Empty rt     = rt
join lt    Empty  = lt
join lt    rt     = Node x' lt rt'
  where (x',rt') = split rt

-- removes and returns minimum element from tree
split :: BST a -> (a,BST a)
split (Node x Empty rt)  = (x,rt)
split (Node x lt    rt)  = (x',Node x lt' rt)
  where (x',lt') = split lt 

-------------------------------------------------------------------------------
-- Order
-------------------------------------------------------------------------------

minim :: BST a -> a
minim Empty              = error "minim on empty tree"
minim (Node x Empty rt)  = x
minim (Node x lt rt)     = minim lt

maxim :: BST a -> a
maxim Empty              = error "maxim on empty tree"
maxim (Node x lt Empty)  = x
maxim (Node x lt rt)     = maxim rt

-------------------------------------------------------------------------------
-- Traversals
-------------------------------------------------------------------------------

inOrder :: BST a -> [a]
inOrder t = aux t []
  where
    aux Empty          xs  = xs
    aux (Node x lt rt) xs  = aux lt (x : aux rt xs)

preOrder :: BST a -> [a]
preOrder t  = aux t []
  where
    aux Empty          xs  = xs
    aux (Node x lt rt) xs  = x : aux lt (aux rt xs)
 
postOrder :: BST a -> [a]
postOrder t  = aux t []
  where
    aux Empty          xs  = xs
    aux (Node x lt rt) xs  = aux lt (aux rt (x:xs))


-------------------------------------------------------------------------------
-- Invariants
-------------------------------------------------------------------------------

isBST :: (Ord a) => BST a -> Bool
isBST Empty          =  True
isBST (Node x lt rt) =  forAll (<x) lt && forAll (>=x) rt
                      && isBST lt && isBST rt
  where
    forAll :: (a -> Bool) -> BST a -> Bool
    forAll p Empty          =  True
    forAll p (Node x lt rt) =  forAll p lt && p x && forAll p rt


-------------------------------------------------------------------------------
-- Generating arbritray Binary Search Trees
-------------------------------------------------------------------------------

instance (Ord a, Arbitrary a) => Arbitrary (BST a) where
  arbitrary = do
    xs <- arbitrary
    return (mkBST xs)

-------------------------------------------------------------------------------
-- Drawing a BST
-------------------------------------------------------------------------------

instance Subtrees (BST a) where
  subtrees Empty           = []
  subtrees (Node x lt rt)  = [lt,rt]  

instance (Show a) => ShowNode (BST a) where
  showNode (Node x lt rt)  = show x

drawOnWith :: FilePath -> (a -> String) -> BST a -> IO ()
drawOnWith file toString = _drawOnWith file showBST
 where
  showBST (Node x _ _) = toString x

-------------------------------------------------------------------------------
-- Pretty Printing a BST 
-- (adapted from http://stackoverflow.com/questions/1733311/pretty-print-a-tree)
-------------------------------------------------------------------------------

pretty :: (Show a) => BST a -> IO ()
pretty t = putStrLn (unlines xss)
 where
   (xss,_,_,_) = pprint t

pprint Empty                =  ([], 0, 0, 0)
pprint (Node x Empty Empty) =  ([s], ls, 0, ls-1)
  where
    s = show x
    ls = length s
pprint (Node x lt rt)         =  (resultLines, w, lw'-swl, totLW+1+swr)
  where 
    nSpaces n = replicate n ' '
    nBars n = replicate n '_'
    -- compute info for string of this node's data
    s = show x
    sw = length s
    swl = div sw 2
    swr = div (sw-1) 2
    (lp,lw,_,lc) = pprint lt
    (rp,rw,rc,_) = pprint rt
    -- recurse
    (lw',lb) = if lw==0 then (1," ") else (lw,"/")
    (rw',rb) = if rw==0 then (1," ") else (rw,"\\")
    -- compute full width of this tree
    totLW = maximum [lw', swl,  1]
    totRW = maximum [rw', swr, 1]
    w = totLW + 1 + totRW
{-
A suggestive example:
     dddd | d | dddd__
        / |   |       \
      lll |   |       rr
          |   |      ...
          |   | rrrrrrrrrrr
     ----       ----           swl, swr (left/right string width (of this node) before any padding)
      ---       -----------    lw, rw   (left/right width (of subtree) before any padding)
     ----                      totLW
                -----------    totRW
     ----   -   -----------    w (total width)
-}
    -- get right column info that accounts for left side
    rc2 = totLW + 1 + rc
    -- make left and right tree same height        
    llp = length lp
    lrp = length rp
    lp' = if llp < lrp then lp ++ replicate (lrp - llp) "" else lp
    rp' = if lrp < llp then rp ++ replicate (llp - lrp) "" else rp
    -- widen left and right trees if necessary (in case parent node is wider, and also to fix the 'added height')
    lp'' = map (\s -> if length s < totLW then nSpaces (totLW - length s) ++ s else s) lp'
    rp'' = map (\s -> if length s < totRW then s ++ nSpaces (totRW - length s) else s) rp'
    -- first part of line1
    line1 = if swl < lw' - lc - 1 then
                nSpaces (lc + 1) ++ nBars (lw' - lc - swl) ++ s
            else
                nSpaces (totLW - swl) ++ s
    -- line1 right bars
    lline1 = length line1
    line1' = if rc2 > lline1 then
                line1 ++ nBars (rc2 - lline1)
             else
                line1
    -- line1 right padding
    line1'' = line1' ++ nSpaces (w - length line1')
    -- first part of line2
    line2 = nSpaces (totLW - lw' + lc) ++ lb 
    -- pad rest of left half
    line2' = line2 ++ nSpaces (totLW - length line2)
    -- add right content
    line2'' = line2' ++ " " ++ nSpaces rc ++ rb
    -- add right padding
    line2''' = line2'' ++ nSpaces (w - length line2'')
    resultLines = line1'' : line2''' : zipWith (\lt rt -> lt ++ " " ++ rt) lp'' rp''





