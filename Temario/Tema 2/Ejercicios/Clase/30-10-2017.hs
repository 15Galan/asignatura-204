-- Función identidad
id :: [a] -> [a]
id = foldr (:) [ ]


-- qSort mejorado
qSort :: (Ord a) => [a] -> [a]
qSort [ ] = [ ]
qSort (h:xs) = qSort ys ++ h : qSort zs
    where
        (ys,zs) = partir h xs


partir :: Ord a => a -> [a] -> ([a],[a])
partir p [ ]    = ([ ],[ ])
partir p (x:xs) | x < p     = (x:us,vs)
                | otherwise = (us,x:vs)
    where
        (us,vs) = partir p xs


-- qSort mejorado (mejorado)
partir' :: Ord a => a -> [a] -> ([a],[a])
partir' p = foldr f ([ ],[ ])
    where
        f e (us,vs) | e < p     = (e:us,vs)
                    | otherwise = (us,e:vs)