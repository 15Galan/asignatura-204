-------------------------------------------------------------------------------
-- Demos for WB Leftist Heaps
--
-- Data Structures. Grado en Informática. UMA.
-- Pepe Gallardo, 2011
-------------------------------------------------------------------------------

module Demos.Heap.WBLeftistHeapDemo where

import Data.List(nub)
import DataStructures.Heap.WBLeftistHeap
import DataStructures.Util.Random
import DataStructures.Graphics.DrawTrees

drawHeap :: (Show a) => Heap a -> IO ()
drawHeap = drawOn "WBLeftistHeap.svg"

outlineHeap :: Heap a -> IO ()
outlineHeap = outlineOn "WBLeftistHeap.svg"

drawCharHeap :: String -> IO ()
drawCharHeap xs = drawOnWith "WBLeftistHeap.svg" (\x -> [x]) (mkHeap xs)

randomHeap :: Int -> Seed -> Heap Int
randomHeap sz seed = mkHeap (take sz . nub . randoms $ seed)

demo1 sz seed = outlineHeap (randomHeap sz seed)

demo2 xs = drawHeap (mkHeap xs)

demo3 = drawCharHeap "murcielago"
