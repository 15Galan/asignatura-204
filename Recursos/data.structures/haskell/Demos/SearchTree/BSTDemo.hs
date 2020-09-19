-------------------------------------------------------------------------------
-- Demos for Binary Search Trees
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2012
-------------------------------------------------------------------------------

module Demos.SearchTree.BSTDemo where

import Data.List(nub)
import DataStructures.SearchTree.BST
import DataStructures.Util.Random
import DataStructures.Graphics.DrawTrees

drawBST :: (Show a) => BST a -> IO ()
drawBST = drawOn "BST.png"

outlineBST :: BST a -> IO ()
outlineBST = outlineOn "BST.png"

drawCharBST :: BST Char ->  IO ()
drawCharBST = drawOnWith "BST.png" (\x -> [x])

randomBST :: Int -> Seed -> BST Int
randomBST sz seed = mkBST (take sz . nub . randoms $ seed)


demo xs = drawBST (mkBST xs)

demo1 sz seed = outlineBST (randomBST sz seed)

demo2 xs = outlineBST (mkBST xs)

demo3 = drawCharBST (mkBST "murcielago")


