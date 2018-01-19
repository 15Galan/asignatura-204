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

  , isBST
  , mkBST

  , pretty
  , drawOnWith
  ) where

import Data.Maybe(isJust)
import DataStructures.Graphics.DrawTrees
import Test.QuickCheck

data BST a  = Empty
            | Node a (BST a) (BST a)
            deriving Show

empty :: BST a
empty  = Empty

isEmpty :: BST a -> Bool
isEmpty Empty  = True
isEmpty _      = False

-------------------------------------------------------------------------------
-- Size
-------------------------------------------------------------------------------

size :: BST a -> Int
size Empty           = 0
size (Node _ lt rt)  = 1 + size lt + size rt

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
isElem x t  = isJust (search x t)


-------------------------------------------------------------------------------
-- Insertion
-------------------------------------------------------------------------------

insert :: (Ord a) => a -> BST a -> BST a
insert x' Empty  =  Node x' Empty Empty
insert x' (Node x lt rt)
    | x'<x       = Node x (insert x' lt) rt
    | x'>x       = Node x lt (insert x' rt)
    | otherwise  = Node x' lt rt

-- Should not modify key in node
updateOrInsert :: (Ord a) => (a -> a) -> a -> BST a -> BST a
updateOrInsert f x' Empty  = Node x' Empty Empty
updateOrInsert f x' (Node x lt rt)
  | x'<x                   = Node x (updateOrInsert f x' lt) rt
  | x'>x                   = Node x lt (updateOrInsert f x' rt)
  | otherwise              = Node (f x) lt rt

-- insert could be written by using updateOrInsert:
-- insert x' = updateOrInsert (const x') x'

mkBST :: (Ord a) => [a] -> BST a
mkBST xs  = foldl (flip insert) empty xs

-------------------------------------------------------------------------------
-- Deletion
-------------------------------------------------------------------------------

delete :: (Ord a) => a -> BST a -> BST a
delete x' Empty  =  Empty
delete x' (Node x lt rt)
  | x'<x         =  Node x (delete x' lt) rt
  | x'>x         =  Node x lt (delete x' rt)
  | otherwise    =  combine lt rt

combine :: BST a -> BST a -> BST a
combine Empty rt     = rt
combine lt    Empty  = lt
combine lt    rt     = Node x' lt rt'
  where (x',rt')  = split rt

-- removes and returns minimum element from non-empty tree
split :: BST a -> (a,BST a)
split (Node x Empty rt)  = (x,rt)
split (Node x lt    rt)  = (x',Node x lt' rt)
  where (x',lt')  = split lt

deleteMinim :: BST a -> BST a
deleteMinim Empty              = error "deleteMinim on empty tree"
deleteMinim (Node x Empty rt)  = rt
deleteMinim (Node x lt rt)     = Node x (deleteMinim lt) rt

deleteMaxim :: BST a -> BST a
deleteMaxim Empty              = error "deleteMaxim on empty tree"
deleteMaxim (Node x lt Empty)  = lt
deleteMaxim (Node x lt rt)     = Node x lt (deleteMaxim rt)

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
inOrder t  = aux t []
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


traversal :: ((b -> b) -> (b -> b) -> (b -> b) -> (b -> b)) ->
             (a -> b -> b) -> b -> BST a -> b
traversal order f z t  = aux t z
  where
    aux Empty           = id
    aux (Node x lt rt)  = order (f x) (aux lt) (aux rt)


foldInOrder :: (a -> b -> b) -> b -> BST a -> b
foldInOrder  = traversal (\xf lf rf -> lf . xf . rf)

foldPreOrder :: (a -> b -> b) -> b -> BST a -> b
foldPreOrder  = traversal (\xf lf rf -> xf . lf . rf)

foldPostOrder :: (a -> b -> b) -> b -> BST a -> b
foldPostOrder  = traversal (\xf lf rf -> lf . rf . xf)

{-
foldIn f z t = fun [t]
 where
        fun []         = z
        fun (Empty:ts) = fun ts
        fun (Node x Empty Empty:ts) = f x (fun ts)
        fun (Node x lt rt:ts) = fun (lt : Node x Empty Empty : rt : ts)
-}

-------------------------------------------------------------------------------
-- Very general operation:
-- Searchs for x in tree:
--  If node with x' such that x==x' was found:
--    if (f (Just x')) returns Nothing, then x' is deleted from tree
--    if (f (Just x')) returns (Just y), then x' is replaced by y (must not alter order)
--  If node with x such that x==x' wasn't found:
--    if (f Nothing) returns Nothing, then same tree is returned
--    if (f Nothing) returns (Just y), then y is inserted into tree
-------------------------------------------------------------------------------

update :: (Ord a) => a -> (Maybe a -> Maybe a) -> BST a -> BST a
update x f Empty  =
  case f Nothing of
    Nothing -> Empty
    Just x' -> Node x' Empty Empty
update x f (Node x' lt rt)
  | x<x'          = Node x' (update x f lt) rt
  | x>x'          = Node x' lt (update x f rt)
  | otherwise     =
      case f (Just x') of
        Nothing  -> combine lt rt -- delete
        Just x'' -> if x' == x'' then Node x'' lt rt
                    else error "update cannot modify order of element"

-- Many operations can be implemented by using this one:
insert' :: (Ord a) => a -> BST a -> BST a
insert' x  = update x $ maybe (Just x) -- insert if not found
                              (const (Just x)) -- replace with x if found


delete' :: (Ord a) => a -> BST a -> BST a
delete' x  = update x $ maybe Nothing -- do nothing if not found
                              (const Nothing) -- delete if found


-- If x is in tree, it's updated with f. Otherwise, x is inserted
updateOrInsert' :: Ord a => (a -> a) -> a -> BST a -> BST a
updateOrInsert' f x  = update x $ maybe (Just x) -- insert if not found
                                        (Just . f) -- update if found

-- If x' such that x==x' isn't in tree, x it's inserted.
-- If x' such that x==x' is in tree:
--  If p x' then x' is deleted from tree
--  otherwise, x' is updated with f
updateOrDeleteOrInsert :: Ord a => (a -> a) -> (a -> Bool) -> a -> BST a -> BST a
updateOrDeleteOrInsert f p x  = update x $ maybe (Just x)  -- insert if not found
                                                 deleteOrUpdate -- delete or update if found
 where
   deleteOrUpdate x'
    | p x'      = Nothing -- delete
    | otherwise = Just (f x') -- update

-------------------------------------------------------------------------------
-- Invariants
-------------------------------------------------------------------------------

isBST :: (Ord a) => BST a -> Bool
isBST Empty           = True
isBST (Node x lt rt)  = forAll (<x) lt && forAll (>=x) rt
                      && isBST lt && isBST rt
  where
    forAll :: (a -> Bool) -> BST a -> Bool
    forAll p Empty           = True
    forAll p (Node x lt rt)  = forAll p lt && p x && forAll p rt


-------------------------------------------------------------------------------
-- Generating arbritray Binary Search Trees
-------------------------------------------------------------------------------

instance (Ord a, Arbitrary a) => Arbitrary (BST a) where
  arbitrary  = do
    xs <- arbitrary
    return (mkBST xs)

-------------------------------------------------------------------------------
-- Drawing a BST
-------------------------------------------------------------------------------

instance Subtrees (BST a) where
  subtrees Empty           = []
  subtrees (Node x lt rt)  = [lt,rt]

  isEmptyTree  = isEmpty

instance (Show a) => ShowNode (BST a) where
  showNode (Node x lt rt)  = show x

drawOnWith :: FilePath -> (a -> String) -> BST a -> IO ()
drawOnWith file toString  = _drawOnWith file showBST
 where
  showBST (Node x _ _)  = toString x

-------------------------------------------------------------------------------
-- Pretty Printing a BST
-- (adapted from http://stackoverflow.com/questions/1733311/pretty-print-a-tree)
-------------------------------------------------------------------------------

pretty :: (Show a) => BST a -> IO ()
pretty t  = putStrLn (unlines xss)
 where
   (xss,_,_,_) = pprint t

pprint Empty                 = ([], 0, 0, 0)
pprint (Node x Empty Empty)  = ([s], ls, 0, ls-1)
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
