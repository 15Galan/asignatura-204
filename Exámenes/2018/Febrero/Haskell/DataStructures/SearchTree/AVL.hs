-------------------------------------------------------------------------------
-- AVL (Adelson-Velskii and Landis) Trees
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2011
-------------------------------------------------------------------------------

module DataStructures.SearchTree.AVL
  ( AVL
  , empty
  , isEmpty
  , size
  , insert
  , search
  , isElem
  , delete
  , updateOrInsert

  , inOrder
  , preOrder
  , postOrder

  , foldInOrder
  , foldPreOrder
  , foldPostOrder

  , minim
  , maxim
  , deleteMinim
  , deleteMaxim

  , isAVL
  , mkAVL

  , height

  , pretty
  , drawOnWith
  ) where

import Data.Maybe(isJust)
import DataStructures.Graphics.DrawTrees
import Test.QuickCheck

data AVL a  = Empty | Node a Int (AVL a) (AVL a) deriving Show

empty :: AVL a
empty  = Empty

isEmpty :: AVL a -> Bool
isEmpty Empty  = True
isEmpty _      = False

-------------------------------------------------------------------------------
-- Size
-------------------------------------------------------------------------------

size :: AVL a -> Int
size Empty             = 0
size (Node _ _ lt rt)  = 1 + size lt + size rt

-------------------------------------------------------------------------------
-- Search
-------------------------------------------------------------------------------

search :: (Ord a) => a -> AVL a -> Maybe a
search x' Empty  = Nothing
search x' (Node x h lt rt)
  | x'<x         = search x' lt
  | x'>x         = search x' rt
  | otherwise    = Just x

isElem :: (Ord a) => a -> AVL a -> Bool
isElem x t  = isJust (search x t)

-- smart constructor to compute height
node :: a -> AVL a -> AVL a -> AVL a
node x lt rt  = Node x h lt rt
 where h  = 1 + max (height lt) (height rt)

height :: AVL a -> Int
height Empty             = 0
height (Node x h lt rt)  = h


-------------------------------------------------------------------------------
-- Insertion
-------------------------------------------------------------------------------

insert :: (Ord a) => a -> AVL a -> AVL a
insert x'  Empty  = node x' Empty Empty
insert x'  (Node x h lt rt)
  | x'<x          = balance x (insert x' lt) rt
  | x'>x          = balance x lt (insert x' rt)
  | otherwise     = Node x' h lt rt

-- Should not modify key in node
updateOrInsert :: (Ord a) => (a -> a) -> a -> AVL a -> AVL a
updateOrInsert f x' Empty  = node x' Empty Empty
updateOrInsert f x' (Node x h lt rt)
  | x'<x                   = balance x (updateOrInsert f x' lt) rt
  | x'>x                   = balance x lt (updateOrInsert f x' rt)
  | otherwise              = Node (f x) h lt rt

-- insert could be written by using updateOrInsert:
-- insert x' = updateOrInsert (const x') x'

rotR :: AVL a -> AVL a
rotR (Node x h (Node lk lh llt lrt) rt)  =
 node lk llt (node x lrt rt)

rotL :: AVL a -> AVL a
rotL (Node x h lt (Node rk rh rlt rrt))  =
 node rk (node x lt rlt) rrt

rightLeaning :: AVL a -> Bool
rightLeaning (Node x h lt rt)  = height lt <= height rt

leftLeaning :: AVL a -> Bool
leftLeaning (Node x h lt rt)  = height lt >= height rt

balance :: a -> AVL a -> AVL a -> AVL a
balance k lt rt
  | (lh-rh > 1) && leftLeaning lt   = rotR (node k lt rt)
  | (lh-rh > 1)                     = rotR (node k (rotL lt) rt)
  | (rh-lh > 1) && rightLeaning rt  = rotL (node k lt rt)
  | (rh-lh > 1)                     = rotL (node k lt (rotR rt))
  | otherwise                       = node k lt rt
  where lh  = height lt
        rh  = height rt

mkAVL :: (Ord a) => [a] -> AVL a
mkAVL xs  = foldl (flip insert) empty xs


-------------------------------------------------------------------------------
-- Deletion
-------------------------------------------------------------------------------

delete :: (Ord a) => a -> AVL a -> AVL a
delete x' Empty  = Empty
delete x' (Node x h lt rt)
  | x'<x         = balance x (delete x' lt) rt
  | x'>x         = balance x lt (delete x' rt)
  | otherwise    = combine lt rt

combine :: AVL a -> AVL a -> AVL a
combine Empty rt     = rt
combine lt    Empty  = lt
combine lt    rt     = balance x' lt rt'
  where (x',rt')  = split rt

-- removes and returns minimum element from non-empty tree
split :: AVL a -> (a,AVL a)
split (Node x h Empty rt)  = (x,rt)
split (Node x h lt    rt)  = (x',balance x lt' rt)
  where (x',lt')  = split lt

deleteMinim :: AVL a -> AVL a
deleteMinim Empty                = error "deleteMinim on empty tree"
deleteMinim (Node x h Empty rt)  = rt
deleteMinim (Node x h lt rt)     = balance x (deleteMinim lt) rt

deleteMaxim :: AVL a -> AVL a
deleteMaxim Empty                = error "deleteMaxim on empty tree"
deleteMaxim (Node x h lt Empty)  = lt
deleteMaxim (Node x h lt rt)     = balance x lt (deleteMaxim rt)

-------------------------------------------------------------------------------
-- Order
-------------------------------------------------------------------------------

minim :: AVL a -> a
minim Empty                = error "minim on empty tree"
minim (Node x h Empty rt)  = x
minim (Node x h lt rt)     = minim lt

maxim :: AVL a -> a
maxim Empty                = error "maxim on empty tree"
maxim (Node x h lt Empty)  = x
maxim (Node x h lt rt)     = maxim rt

-------------------------------------------------------------------------------
-- Invariants
-------------------------------------------------------------------------------

isAVL :: (Ord a) => AVL a -> Bool
isAVL Empty             = True
isAVL (Node x h lt rt)  = forAll (<x) lt && forAll (>x) rt
                          && abs (height lt - height rt) < 2
                          && isAVL lt && isAVL rt
    where
      forAll :: (a -> Bool) -> AVL a -> Bool
      forAll p Empty             = True
      forAll p (Node x h lt rt)  = forAll p lt && p x && forAll p rt


-------------------------------------------------------------------------------
-- Traversals
-------------------------------------------------------------------------------

inOrder :: AVL a -> [a]
inOrder t  = aux t []
  where
    aux Empty            xs  = xs
    aux (Node x h lt rt) xs  = aux lt (x : aux rt xs)

preOrder :: AVL a -> [a]
preOrder t  = aux t []
  where
    aux Empty            xs  = xs
    aux (Node x h lt rt) xs  = x : aux lt (aux rt xs)

postOrder :: AVL a -> [a]
postOrder t  = aux t []
  where
    aux Empty            xs  = xs
    aux (Node x h lt rt) xs  = aux lt (aux rt (x:xs))

traversal :: ((b -> b) -> (b -> b) -> (b -> b) -> (b -> b)) ->
             (a -> b -> b) -> b -> AVL a -> b
traversal order f z t  = aux t z
  where
    aux Empty             = id
    aux (Node x h lt rt)  = order (f x) (aux lt) (aux rt)


foldInOrder :: (a -> b -> b) -> b -> AVL a -> b
foldInOrder  = traversal (\xf lf rf -> lf . xf . rf)

foldPreOrder :: (a -> b -> b) -> b -> AVL a -> b
foldPreOrder  = traversal (\xf lf rf -> xf . lf . rf)

foldPostOrder :: (a -> b -> b) -> b -> AVL a -> b
foldPostOrder  = traversal (\xf lf rf -> lf . rf . xf)


-------------------------------------------------------------------------------
-- Generating arbritray AVL Trees
-------------------------------------------------------------------------------

instance (Ord a, Arbitrary a) => Arbitrary (AVL a) where
  arbitrary  = do
    xs <- arbitrary
    return (mkAVL xs)

-------------------------------------------------------------------------------
-- pretty printing a tree
-- (adapted from http://stackoverflow.com/questions/1733311/pretty-print-a-tree)
-------------------------------------------------------------------------------

pretty :: (Show a) => AVL a -> IO ()
pretty t  = putStrLn (unlines xss)
 where
   (xss,_,_,_) = pprint' t

pprint' Empty                   =  ([], 0, 0, 0)
pprint' (Node x h Empty Empty)  =  ([s], ls, 0, ls-1)
  where
    s = show x
    ls = length s
pprint' (Node x h l r)          =  (resultLines, w, lw'-swl, totLW+1+swr)
  where
    nSpaces n = replicate n ' '
    nBars n = replicate n '_'
    -- compute info for string of this node's data
    s = show x
    sw = length s
    swl = div sw 2
    swr = div (sw-1) 2
    (lp,lw,_,lc) = pprint' l
    (rp,rw,rc,_) = pprint' r
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
    resultLines = line1'' : line2''' : zipWith (\l r -> l ++ " " ++ r) lp'' rp''

-------------------------------------------------------------------------------
-- Drawing an AVL Tree
-------------------------------------------------------------------------------

instance Subtrees (AVL a) where
  subtrees Empty             = []
  subtrees (Node x h lt rt)  = [lt,rt]

  isEmptyTree  = isEmpty

instance (Show a) => ShowNode (AVL a) where
  showNode (Node x _ _ _)  = show x

drawOnWith :: FilePath -> (a -> String) -> AVL a -> IO ()
drawOnWith file toString  = _drawOnWith file showBST
 where
  showBST (Node x _ _ _)  = toString x
