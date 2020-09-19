-------------------------------------------------------------------------------
-- Demos for AVL Trees
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2011
-------------------------------------------------------------------------------

module Demos.SearchTree.AVLDemo where

import Data.List(nub)
import DataStructures.SearchTree.AVL
import DataStructures.Util.Random
import DataStructures.Graphics.DrawTrees


drawAVL :: (Show a) => AVL a -> IO ()
drawAVL = drawOn "AVL.png"

outlineAVL :: AVL a -> IO ()
outlineAVL = outlineOn "AVL.png"

drawCharAVL :: String ->  IO ()
drawCharAVL xs = drawOnWith "AVL.png"  (\x -> [x]) (mkAVL xs)

randomAVL :: Int -> Seed -> AVL Int
randomAVL sz seed = mkAVL (take sz . nub . randoms $ seed)

demo1 sz seed = outlineAVL (randomAVL sz seed)

demo2 xs = drawAVL (mkAVL xs)

demo3 = drawCharAVL "murcielago"

