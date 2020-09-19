data Set a = Empty | Node a (Set a) deriving Eq -- Porque ya son igualables
-- data Set a = Empty | Node a (Set a) deriving Show


empty :: Set a
empty = Empty


isEmpty :: Set a -> Bool
isEmpty Empty = True
isEmpty _     = False


insert :: (Ord a) => a -> Set a -> Set a
insert x Empty      = Node x Empty
insert x (Node y s) | x < y     = Node x (Node y s)
                    | x == y    = Node y s
                    | otherwise = Node y (insert x s)


delete :: Ord a => a -> Set a -> Set a
delete _ Empty        = Empty
delete x k@(Node y s) | x < y     = k
                      | x == y    = s
                      | otherwise = Node y (delete x s)


isElem :: Ord a => a -> Set a -> Bool
isElem _ Empty      = False
isElem x (Node y s) | x < y     = False
                    | x == y    = True
                    | otherwise = isElem x s


instance Show a => Show (Set a) where
    show s = "("++ intercalate "," (aux s) ++")"
        where
            aux Empty      = [ ]
            aux (Node x s) = show x : aux s






