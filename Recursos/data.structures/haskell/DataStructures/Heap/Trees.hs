module Trees where
import Data.List


--------------------------------------------------
-- Árboles
--------------------------------------------------

data Tree a = Empty | Node a [Tree a] deriving Show

tree1 :: Tree Int
tree1 = Node 1 [ Node 2 [ Node 4 []
                        , Node 5 []
                        , Node 6 []
                        ]
               , Node 3 [ Node 7 []
                        ]
               ]


sumT :: (Num a) => Tree a -> a
sumT Empty = 0
sumT (Node x ts) = x + sum [sumT t | t <- ts]


heightT :: Tree a -> Int
heightT Empty = 0
heightT (Node x []) = 1
heightT (Node x ts) = 1 + maximum [heightT t | t <- ts]



--------------------------------------------------
-- Árboles Binarios
--------------------------------------------------

data TreeB a = EmptyB | NodeB a (TreeB a) (TreeB a) deriving Show

tree2 :: TreeB Int
tree2 = NodeB 1 (NodeB 2 (NodeB 4 EmptyB EmptyB)
                         (NodeB 5 EmptyB EmptyB)
                )
                (NodeB 3 (NodeB 6 EmptyB EmptyB)
                         (EmptyB)
                )


tree3 :: TreeB Int
tree3 = NodeB 1 (NodeB 2 (NodeB 4 EmptyB EmptyB)
                         (NodeB 5 EmptyB EmptyB)
                )
                (NodeB 3 (NodeB 6 EmptyB EmptyB)
                         (NodeB 2 EmptyB EmptyB)
                )

sumB :: (Num a) => TreeB a -> a
sumB EmptyB = 0
sumB (NodeB x lt rt) = x + sumB lt + sumB rt


atLevelB :: Int -> TreeB a -> [a]
atLevelB _ EmptyB = []
atLevel 0 (NodeB x lt rt) = [x]
atLevel n (NodeB x lt rt) = atLevelB (n-1) lt ++ atLevelB (n-1) rt


pathsToB :: (Eq a) => a -> TreeB a -> [[a]]
pathsToB x EmptyB = []
pathsToB x (NodeB y lt rt) | x == y    = [y] : ps
                           | otherwise = ps
    where
        ps = map (y:) (pathsToB x lt ++ pathsToB x rt)


preOrderB :: TreeB a -> [a]
preOrderB EmptyB = []
preOrderB (NodeB x lt rt) = [x] ++ preOrderB lt ++ preOrderB rt


inOrder :: TreeB a -> [a]
inOrder EmptyB = []
inOrder (NodeB x lt rt) = inOrder lt ++ [x] ++ inOrder rt


postOrder :: TreeB a -> [a]
postOrder EmptyB = []
postOrder (NodeB x lt rt) = postOrder lt ++ postOrder rt ++ [x]